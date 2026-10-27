-- case-label.lua – the line "Case: <label>" at the top of every page of a case,
-- and "Case: ..." or "Cases: ..." at the top of the companion modules and of
-- any other page that names its case.
--
-- A page belongs to a case when its source sits under cases/<slug>/: the case
-- pages and the codebooks in cases/<slug>/data/. A companion module names the
-- cases that it works on in companion-cases in _variables.yml, by the name of
-- its file. Any other page, such as a week of the semester track that works on
-- one case, names it in its front matter as `case: <slug>` (or a list of
-- slugs). The label of each case is cases.<slug>.label there, so that the
-- names of the cases are written in one place; a slug without a label gets no
-- line. Each label links to the case study of its case, except on the case
-- study itself. A one-line script moves the line into the title block, above
-- the title; without JavaScript it stays at the top of the content. Styled as
-- .case-label in styles.css. Applies to HTML output only, so the decks are
-- never touched, and it runs on pages served from the freeze cache as well,
-- because a filter runs at every render.

-- _variables.yml read as the metadata of an otherwise empty Markdown document,
-- which is how pandoc parses a YAML block without a YAML library
local function read_variables()
  local dir = quarto.project.directory
  if not dir then return nil end
  local f = io.open(pandoc.path.join({ dir, "_variables.yml" }), "r")
  if not f then return nil end
  local text = f:read("a")
  f:close()
  return pandoc.read("---\n" .. text .. "\n---\n", "markdown").meta
end

-- The path of the page relative to the project, with forward slashes
local function page_path()
  local input = quarto.doc.input_file
  local dir = quarto.project.directory
  if not input or not dir then return nil end
  local rel = pandoc.path.make_relative(input, dir)
  return (rel:gsub("\\", "/"))
end

local MOVE_SCRIPT = [[
<script>
(function () {
  var label = document.currentScript.previousElementSibling;
  var header = document.getElementById("title-block-header");
  if (label && header) header.insertBefore(label, header.firstChild);
})();
</script>
]]

-- The label of one case, linked to its case study unless `here` is true
local function case_inlines(vars, slug, prefix, here)
  local entry = vars.cases and vars.cases[slug]
  if not entry or not entry.label then return nil end
  local label = pandoc.utils.stringify(entry.label)
  if label == "" then return nil end
  if here then return { pandoc.Str(label) } end
  return { pandoc.Link(label, prefix .. "cases/" .. slug .. "/index.qmd", "",
                       { title = "The case study of this case" }) }
end

function Pandoc(doc)
  if not quarto.doc.is_format("html") then return nil end
  local rel = page_path()
  if not rel then return nil end
  local vars = read_variables()
  if not vars then return nil end

  local slugs, prefix, here = {}, "", nil
  local slug, rest = rel:match("^cases/([^/]+)/(.+)$")
  local module = rel:match("^companion/([^/]+)%.qmd$")
  if slug then
    slugs = { slug }
    prefix = string.rep("../", select(2, rest:gsub("/", "")) + 2)
    if rest == "index.qmd" then here = slug end
  elseif module and vars["companion-cases"] and vars["companion-cases"][module] then
    for _, s in ipairs(vars["companion-cases"][module]) do
      table.insert(slugs, pandoc.utils.stringify(s))
    end
    prefix = "../"
  elseif doc.meta.case then
    local named = doc.meta.case
    if pandoc.utils.type(named) == "List" then
      for _, s in ipairs(named) do table.insert(slugs, pandoc.utils.stringify(s)) end
    else
      slugs = { pandoc.utils.stringify(named) }
    end
    prefix = string.rep("../", select(2, rel:gsub("/", "")))
  else
    return nil
  end

  local parts = {}
  for _, s in ipairs(slugs) do
    local inl = case_inlines(vars, s, prefix, s == here)
    if inl then table.insert(parts, inl) end
  end
  if #parts == 0 then return nil end

  local inlines = { pandoc.Str(#parts > 1 and "Cases:" or "Case:"), pandoc.Space() }
  for i, inl in ipairs(parts) do
    if i > 1 then
      table.insert(inlines, pandoc.Str(","))
      table.insert(inlines, pandoc.Space())
    end
    for _, x in ipairs(inl) do table.insert(inlines, x) end
  end

  local line = pandoc.Div({ pandoc.Plain(inlines) }, pandoc.Attr("", { "case-label" }))
  table.insert(doc.blocks, 1, pandoc.RawBlock("html", MOVE_SCRIPT))
  table.insert(doc.blocks, 1, line)
  return doc
end

local Parser = require("parser")
local Codegen = require("codegen")

local Compiler = {}

function Compiler.compile(code, filename)
    if filename == nil then filename = "main" end
    local cg = Codegen.new()
    local parser = Parser.new(code, cg, filename)

    local function on_error(info)
        print("Complation Aborted! \n" .. info .. "\nfile: " .. filename .. "\nline:" 
            .. parser.line .. "," .. parser.line_pos .. "\n" .. debug.traceback())
    end

    xpcall(function() Parser.parse(parser) end, on_error)

    return cg
end

local testcode = [[
function clamp(x :float, lower :float, upper :float) :float
    return math.max(math.min(x, upper), lower)
end

function smoothstep(edge0 :float, edge1 :float, x :float) :float
    local t:float = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0)
    return t * t * (3.0 - 2.0 * t)
end

function dot(p0 <array> :float3, p1 <array> :float3) :float
    local sum :float3 = p0 * p1
    return sum.x + sum.y + sum.z
end

function normalize(p <array> :float3) :float3
    local sum :float = math.sqrt(dot(p, p))
    return p / sum
end
]]

local result = Compiler.compile(testcode)
print(result:export_code("dot"))
print(result:export_code("normalize"))
print(result:export_code("smoothstep"))
local smoothstep = result:export("smoothstep")
local dot = result:export("dot")
local normalize = result:export("normalize")

local Utils = require("utils")
local table2sexpr = Utils.table2sexpr

print(smoothstep(40, 50, 46))
print(dot({1,2,3}, {1,2,3}))
print(table2sexpr(normalize({1, 2, 3})))

return Compiler

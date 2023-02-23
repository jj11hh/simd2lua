local Parser = require("parser")
local Codegen = require("codegen")

local Compiler = {}

function Compiler.compile(code, filename)
    if filename == nil then filename = "main" end
    local cg = Codegen.new()
    local parser = Parser.new(code, cg, filename)

    local function on_error(info)
        error("Complation Aborted! \n" .. info .. "\nfile: " .. filename .. "\nline:" 
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

function dot(x0 :float, y0 :float, z0 :float, x1 :float, y1 :float, z1 :float) :float
    return x0*x1 + y0*y1 + z0*z1
end
]]

local result = Compiler.compile(testcode)
local smoothstep = result:export("smoothstep")
local dot = result:export("dot")

print(result:export_code("dot"))
print(smoothstep(40, 50, 46))
print(dot(1,2,3,1,2,3))


return Compiler

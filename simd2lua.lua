local Parser = require("parser")
local Codegen = require("codegen")

local Compiler = {}

function Compiler.compile(code, filename)
    if filename == nil then filename = "main" end
    local cg = Codegen.new()
    local parser = Parser.new(code, cg, filename)
    local message = nil

    local function on_error(info)
        message = "Complation Aborted! \n" .. info .. "\nfile: " .. filename .. "\nline:" 
            .. parser.line .. "," .. parser.line_pos .. "\n" .. debug.traceback()
    end

    xpcall(function() Parser.parse(parser) end, on_error)

    assert(message == nil, message)

    return cg
end
return Compiler

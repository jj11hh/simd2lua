local pprint = require("pprint")

-- See also Page 22 in Programming in Lua, 4th Editor
local binary_operators = {
    { "or" },
    { "and" },
    { "<", ">", "<=", ">=", "~=", "==" },
    { "|" },
    { "~" },
    { "&" },
    { "<<", ">>" },
    -- { ".." },
    { "+", "-" },
    { "*", "/", "//", "%" },
}

local unary_operators = {
    { "not", "-", "#", "~"},
}

local Lexer = require("lexer")
local take = Lexer.next

local Parser = {}

function Parser.new(input, handler)
    local self = Lexer.new(input) -- Inherit from lexer
    self.handler = handler
    return self
end

local function syntax_error(parser, message)
    error("syntax error: " .. message .. " at " .. parser.filename .. ":" .. parser.line)
end

local function unexpect(self, token)
    if self.current == token then
        syntax_error(self, "unexpected " .. token)
    end
end

local function expect(parser, token)
    if parser.current ~= token then
        syntax_error(parser, "expecting " .. token .. ", get " .. tostring(parser.current))
    end
end

local function syntax_assert(parser, cond, message)
    if not cond then
        syntax_error(parser, message)
    end
end

local function expect_and_take(parser, token)
    expect(parser, token)
    take(parser)
end

local function table_find(t, ele)
    for i = 1,#t do
        if ele == t[i] then return i end
    end

    return nil
end

local string_match = string.match

local parse_atom
local parse_postfix
local parse_expr
local parse_parameters
local parse_unary
local parse_binary
local parse_binary_
local parse_code_block
local parse_param_decl
local is_identifier

local if_ends = {"end", "else", "elseif"}

is_identifier = function(token)
    return string_match(token, "^[a-zA-Z_][a-zA-Z0-9_]*") ~= nil
end

local valid_types = {
    float = true, float2 = true, float3 = true, float4 = true,
    int = true, int2 = true, int3 = true, int4 = true,
    -- TODO: Matrix types
}

local function is_valid_type(token)
    return valid_types[token] == true
end

local function parse_type_notation(self)
    expect_and_take(self, ":")
    local token = take(self)
    syntax_assert(self, is_valid_type(token), "expecting type name, get " .. token)
    return token
end

local function parse_attributes(self)
    local attributes = {}
    if self.current == "<" then
        take(self)
        while is_identifier(self.current) do
            attributes[#attributes+1] = self.current
            take(self)
        end
        expect_and_take(self, ">")
    end
    return attributes
end

local function parse_variable_decl(self)
    expect_and_take(self, "local")
    local varname = take(self)
    syntax_assert(self, is_identifier(varname), "expecting identifier, get " .. varname)
    local attributes = parse_attributes(self)
    local vartype = parse_type_notation(self)
    local init
    
    if self.current == "=" then
        take(self)
        init = parse_expr(self)
    end

    self.handler.variable_decl(varname, attributes, vartype, init)
end

parse_param_decl = function(self)
    local params = {}

    while true do
        local token = self.current
        if not is_identifier(token) then break end
        take(self)
        local name = token
        token = self.current
        
        local attributes = parse_attributes(self)
        local type_notation = parse_type_notation(self)

        -- Type notation
        local param = {
            name = name, type = type_notation, attributes = attributes
        }
        params[#params+1] = param
    end

    pprint(params)

    return params
end

parse_code_block = function(self, ends, ends_with_nil)
    local handler = self.handler
    local cond
    local token
    local ends_with
    if ends == nil then ends = {"end"} end
    while table_find(ends, self.current) == nil do
        if ends_with_nil then
            if self.current == nil then break end
        else
            unexpect(self, nil)
        end

        token = self.current
        if token == "if" then
            take(self)
            cond = parse_expr(self)
            expect_and_take(self, "then")
            handler.process_if(cond)
            while true do
                ends_with = parse_code_block(self, if_ends)
                if ends_with == "end" then 
                    handler.end_if(cond)
                    break 
                elseif ends_with == "elseif" then
                    cond = parse_expr(self)
                    handler.process_elseif(cond)
                elseif ends_with == "else" then
                    handler.process_else()
                end
            end
        elseif token == "while" then
            take(self)
            cond = parse_expr(self)
            expect_and_take(self, "do")
            handler.process_while(cond)
            parse_code_block(self)
            handler.end_while()
        elseif token == "for" then
            take(self)
            local iterator_name = self.current
            syntax_assert(type(iterator_name) ~= "string", "expecting iterator name, get " .. tostring(iterator_name))
            take(self)
            expect_and_take(self, "=")
            local begin = parse_expr(self)
            expect_and_take(self, ",")
            local ends = parse_expr(self)
            local step = 1
            if self.current == "," then
                take(self)
                step = parse_expr(self)
            end
            expect_and_take(self, "do")
            handler.process_for(iterator_name, begin, ends, step)
            parse_code_block(self)
            handler.end_for()
        elseif token == "function" then
            take(self)
            local function_name = take(self)
            syntax_assert(type(iterator_name) ~= "string", "expecting function name, get " .. tostring(iterator_name))

            expect_and_take(self, "(")
            local params = parse_param_decl(self)
            expect_and_take(self, ")")

            local attributes = parse_attributes(self)
            local return_type = parse_type_notation(self)

            handler.process_function_decl(function_name, params, attributes, return_type)
            parse_code_block(self)
            handler.end_function()
        elseif token == "local" then
            parse_variable_decl(self)
        elseif token == "return" then
            take(self)
            handler.return_val(parse_expr(self))
        else -- Function call or assignment
            local lvalue = parse_expr(self)
            if self.current == "=" then -- assignment
                take(self)
                local rvalue = parse_expr(self)
                handler.assign(lvalue, rvalue)
            else
                handler.expr_statement(lvalue)
            end
        end
    end
    return take(self)
end

-- Left recursion elimination
-- E := E + T | T
-- ==>
--    E  := T E'
--    E' := nil | + E E'

parse_binary = function(self, level)
    if level == nil then level = 1 end
    local ops = binary_operators[level]

    if ops == nil then
        return parse_unary(self)
    end

    local a = parse_binary(self, level+1)
    return parse_binary_(self, level, ops, a)
end

parse_expr = parse_binary

parse_binary_ = function(self, level, ops, a)
    local token = self.current
    local handler = self.handler
    if table_find(ops, token) ~= nil then
        take(self)
        local b = parse_binary(self, level+1)
        return parse_binary_(self, level, ops, handler.binary(token, a, b))
    else
        return a
    end
end

parse_unary = function(self, level)
    if level == nil then level = 1 end
    local ops = unary_operators[level]

    if ops == nil then
        return parse_atom(self)
    end

    local handler = self.handler
    local token = self.current

    if table_find(ops, token) ~= nil then
        take(self)
        return handler.unary(token, parse_unary(self, level+1))
    else
        return parse_unary(self, level+1)
    end
end

parse_atom = function(self)
    local token = self.current
    local handler = self.handler
    if token == "(" then
        local expr = parse_expr(self)
        expect_and_take(")")
        return expr
    elseif type(token) == "string" then
        local id = handler.identifier(token)
        take(self)
        return parse_postfix(self, id)
    elseif type(token) == "number" or type(token) == "boolean" then
        take(self)
        return token
    end

    syntax_error(self, "expecting atom, get " .. tostring(token))
end

local function parse_parameters(self)
    local params = {}
    while true do
        params[#params+1] = parse_expr(self)

        if self.current == "," then
            take(self)
        else
            break
        end
    end

    return params
end

parse_postfix = function(self, id)
    local token = self.current
    local handler = self.handler

    if token == "." then
        take(self)
        token = self.current
        take(self)
        return handler.field_access(self, id, token)
    elseif token == "[" then
        take(self)
        local index = parse_expr(self)
        expect_and_take(self, "]")
        return handler.index_access(self, id, index)
    elseif token == "(" then
        take(self)
        local parameters = parse_parameters(self)
        expect_and_take(self, ")")
        return handler.call(id, parameters)
    end

    return id
end

-- Tests

local function test_expr()
    local code = "1 and 2 * 2 - 3 + -2e-3 * not 1 ~ ~0xFF"
    local handler = {}
    handler.binary = function (op, a, b)
        local r = table.concat{"(", op, " ", a, " ", b, ")"}
        print(r)
        return r
    end

    handler.unary = function (op, a)
        local r = table.concat{"(", op, " ", a, ")"}
        print(r)
        return r
    end

    local parser = Parser.new(code, handler)
    assert(parse_binary(parser) == "(and 1 (~ (+ (- (* 2 2) 3) (* (- 0.002) (not 1))) (~ 255)))")
end


local function test_function()
    local meta = {}
    meta.__index = function(t, key)
        local function handle(...)
            local output = {"(", key, ", "}
            local args = {...}

            for i=1,#args do
                output[#output+1] = tostring(args[i])
                output[#output+1] = ", "
            end
            output[#output] = ")"

            local repr = table.concat(output)
            print(repr)

            return repr
        end
        return handle
    end

    local handler = {}
    setmetatable(handler, meta)

    local code = [[
        function test1(i <inout> :int) :int
            return i * i
        end

        function test2() :int
            local temp :int = 2
            return test1(3 + 3)
        end
    ]]

    local parser = Parser.new(code, handler)
    parse_code_block(parser, nil, true)
end

test_expr()
test_function()

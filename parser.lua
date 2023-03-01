-- local pprint = require("pprint")

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

local valid_types = {
    void = true,
    float = true, float2 = true, float3 = true, float4 = true,
    int = true, int2 = true, int3 = true, int4 = true,
    bool = true, bool2 = true, bool3 = true, bool4 = true,
    -- TODO: Matrix types
}

local Lexer = require("lexer")
local take = Lexer.next

local Parser = {}

function Parser.new(input, handler)
    local self = Lexer.new(input) -- Inherit from lexer
    self.handler = handler
    return self
end

function Parser.set_handler(self, handler)
    self.handler = handler
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

local Utils = require("utils")
local table2sexpr = Utils.table2sexpr
local table_find = Utils.table_find
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

local if_ends = {"end", "else", "elseif"}

local function is_identifier(token)
    return string_match(token, "^[a-zA-Z_][a-zA-Z0-9_]*") ~= nil
end

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

    self.handler:decl_variable(varname, attributes, vartype, init)
end

parse_param_decl = function(self)
    local handler = self.handler
    local params = {}

    while true do
        local token = self.current
        if not is_identifier(token) then break end
        take(self)
        local name = token
        token = self.current
        
        local attributes = parse_attributes(self)
        local type_notation = parse_type_notation(self)
        params[#params+1] = handler:decl_param(name, attributes, type_notation)

        token = self.current
        if token == "," then
            take(self)
        end
    end

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
            handler:begin_if(cond)
            while true do
                ends_with = parse_code_block(self, if_ends)
                if ends_with == "end" then 
                    handler:end_if(cond)
                    break 
                elseif ends_with == "elseif" then
                    cond = parse_expr(self)
                    expect_and_take(self, "then")
                    handler:begin_elseif(cond)
                elseif ends_with == "else" then
                    handler:begin_else()
                end
            end
        elseif token == "while" then
            take(self)
            cond = parse_expr(self)
            expect_and_take(self, "do")
            handler:begin_while(cond)
            parse_code_block(self)
            handler:end_while()
        elseif token == "for" then
            take(self)
            local iterator_name = self.current
            syntax_assert(self, type(iterator_name) == "string", "expecting iterator name, get " .. tostring(iterator_name))
            take(self)
            expect_and_take(self, "=")
            local begin = parse_expr(self)
            expect_and_take(self, ",")
            local ends = parse_expr(self)
            local step = handler:number_literal(1)
            if self.current == "," then
                take(self)
                step = parse_expr(self)
            end
            expect_and_take(self, "do")
            handler:begin_for(iterator_name, begin, ends, step)
            parse_code_block(self)
            handler:end_for()
        elseif token == "function" then
            take(self)
            local function_name = take(self)
            syntax_assert(self, type(iterator_name) ~= "string", "expecting function name, get " .. tostring(iterator_name))

            expect_and_take(self, "(")
            local params = parse_param_decl(self)
            expect_and_take(self, ")")

            local attributes = parse_attributes(self)
            local return_type = parse_type_notation(self)

            handler:decl_function(function_name, params, attributes, return_type)
            parse_code_block(self)
            handler:end_function()
        elseif token == "local" then
            parse_variable_decl(self)
        elseif token == "return" then
            take(self)
            if table_find(ends, self.current) ~= nil or self.current == nil then
                handler:return_val()
            else
                handler:return_val(parse_expr(self))
            end
            -- Control flow should terminated here
            syntax_assert(self, table_find(ends, self.current) ~= nil, "end of block expected")
        elseif token == "break" then
            take(self)
            handler:handle_break()
        elseif token == "goto" then
            take(self)
            local label = take(self)
            handler:handle_goto(label)
        elseif token == "::" then
            take(self)
            local label = take(self)
            handler:label(label)
            expect_and_take(self, "::")
        else -- Function call or assignment
            local lvalue = parse_expr(self)
            if self.current == "=" then -- Assignment
                take(self)
                local rvalue = parse_expr(self)
                handler:assign(lvalue, rvalue)
            else
                handler:expr_statement(lvalue)
            end
        end
    end
    return take(self)
end

function Parser.parse(self)
    return parse_code_block(self, nil, true)
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
        return parse_binary_(self, level, ops, handler:binary(token, a, b))
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
        return handler:unary(token, parse_unary(self, level+1))
    else
        return parse_unary(self, level+1)
    end
end

parse_atom = function(self)
    local token = self.current
    local handler = self.handler
    if token == "(" then
        take(self)
        local expr = parse_expr(self)
        expect_and_take(self, ")")
        return parse_postfix(self, expr)
    elseif type(token) == "string" then
        local id = handler:identifier(token)
        take(self)
        return parse_postfix(self, id)
    elseif type(token) == "number" then
        take(self)
        return handler:number_literal(token)
    elseif type(token) == "boolean" then
        take(self)
        return handler:bool_literal(token)
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
        return parse_postfix(self, handler:get_field(id, token))
    elseif token == "[" then
        take(self)
        local index = parse_expr(self)
        expect_and_take(self, "]")
        return parse_postfix(self, handler:get_index(id, index))
    elseif token == "(" then
        take(self)
        local parameters = parse_parameters(self)
        expect_and_take(self, ")")
        return parse_postfix(self, handler:call(id, parameters))
    end

    return id
end

-- Tests
--[=====[
local function test_expr()
    local code = "1 and 2 * 2 - 3 + -2e-3 * not 1 ~ ~0xFF"
    local handler = {}
    handler.binary = function (_, op, a, b)
        local r = table.concat{"(", op, " ", a, " ", b, ")"}
        print(r)
        return r
    end

    handler.unary = function (_, op, a)
        local r = table.concat{"(", op, " ", a, ")"}
        print(r)
        return r
    end

    handler.number_literal = function (_, i) return i end
    handler.bool_literal = function (_, i) return i end

    local parser = Parser.new(code, handler)
    assert(parse_binary(parser) == "(and 1 (~ (+ (- (* 2 2) 3) (* (- 0.002) (not 1))) (~ 255)))")
end

local function test_function()
    local meta = {}
    meta.__index = function(t, key)
        local function handle(...)
            local output = {"(", key, " "}
            local args = {...}

            for i=2,#args do
                output[#output+1] = table2sexpr(args[i])
                output[#output+1] = " "
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
        function test1(i <inout> :int) <pure> :int
            return i * i
        end

        function test2() :int
            local temp <const> :float4 = float4(1.0, 2.0 * 2.0, 3.0 % 4.0, 5.5)
            for i=1,10,2 do
                temp = 3.0 + temp * (temp).xyz ~ i
            end
            func(i * i)
            math.sin(pow(i, 2.0))
            return test1(3 + 3).xyzw[0]
        end
    ]]

    local parser = Parser.new(code, handler)
    Parser.parse(parser)
end

test_expr()
test_function()
--]=====]

return Parser

-- Lexer for simd2lua

local string_find = string.find
local string_match = string.match
local string_sub = string.sub
local table_concat = table.concat

local Lexer = {}

function Lexer.new(input, filename)
    if filename == nil then filename = "" end
    local self = {
        input = input,
        position = 1,
        current = nil,
        backtrace = {},
        line = 1,
        line_pos = 1,
        filename = filename,
    }

    Lexer.next(self) -- Parse first token

    return self
end

local function skip_blanks(self)
    local pos = self.position
    local line = self.line
    local line_pos = self.line_pos
    local input = self.input
    local in_comment = false

    while pos <= #input do
        local char = string.sub(input, pos, pos)
        local is_blank = char == " " or char == "\t" or char == "\r" or char == "\n"

        if char == "-" then -- Maybe comment
            if pos+1 <= #input and string.sub(input, pos+1, pos+1) == "-" then
                in_comment = true
            end
        end

        if not in_comment and not is_blank then break end

        if char == "\n" then
            line = line + 1
            line_pos = 0
            in_comment = false
        else
            line_pos = line_pos + 1
        end

        pos = pos + 1
    end

    return pos, line, line_pos
end

local operators = {
    ["+"] = false,
    ["-"] = false,
    ["*"] = false,
    ["/"] = {"//"},
    ["^"] = false,
    ["%"] = false,
    ["="] = {"=="},
    ["~"] = {"~="},
    ["<"] = {"<=", "<<"},
    [">"] = {">=", ">>"},
    ["#"] = false,
    ["&"] = false,
    ["|"] = false,
}

local constants = {
    ["true"] = true,
    ["false"] = false,
}

local function lex_error(self, message)
    local error_message = {message, " at ", self.filename, ":", self.line, ", character ", tostring(self.line_pos)}
    error(table_concat(error_message))
end

-- Copy from https://github.com/GavinHigham/lpil53/blob/master/lexer.lua, with some modification
local function lex_numeric(input, init)
    local i, j, token = string_find(input, '^(0[xX]%x*%.?%x+)', init) --Check for hexidecimal constants
	local exponent = '^([pP][+-]?%x+)' --Pattern for binary exponent

	if not i then
        i, j, token = string_find(input, '^(%d*%.?%d+)', init) --Check for numeric constants
		if i then
            exponent = '^([eE][+-]?%d+)' --Pattern for decimal exponent
		end
    end

    if i then
		local i2, j2, token2 = string_find(input, exponent, j+1) --Check for trailing exponent
		if i2 then
			j, token = j2, token..token2
		end
	end
	return i, j, token
end

local function lexer_lex(self)
    local chars = {}
    local pos, line, line_pos = skip_blanks(self)
    local input = self.input
    local old_pos = pos

    if pos > #input then
        self.current = nil
        return nil, pos, line, line_pos
    end

    local char = string_sub(input, pos, pos)
    local token = char

    if string_match(char, "[a-zA-Z_]") then -- Most time here
        local i, j = string_find(input, "^[0-9a-zA-Z_]+", pos)
        token = string_sub(input, i, j)
        pos = j+1
        local constant = constants[token]
        if constant ~= nil then token = constant end
    else 
        local op = operators[char]
        if op ~= nil then -- Operators
            if op == false then -- Single char op
                token = char
                pos = pos + 1
            else -- Double char op
                local mb = string_sub(input, pos, pos+1) -- No need for boundary check
                local found = false
                for i=1,#op do
                    if mb == op[i] then
                        token = mb
                        pos = pos + #mb
                        found = true
                        break
                    end
                end
                if not found then 
                    pos = pos + 1 
                end
            end
        else
            local i, j, numeric = lex_numeric(input, pos) -- Try get numeric literal
            if numeric ~= nil then
                pos = j+1
                token = tonumber(numeric) -- Parse number out
            else -- Output current char
                pos = pos+1
            end
        end
    end

    line_pos = line_pos + (pos - old_pos)
    return token, pos, line, line_pos
end

function Lexer.peek(self)
    return self.current, self.position, self.line, self.line_pos
end

function Lexer.next(self)
    local current = self.current
    local old_pos = self.position
    local old_line = self.line
    local old_line_pos = self.line_pos

    local token, new_pos, line, line_pos = lexer_lex(self)

    self.current = token
    self.position = new_pos
    self.line = line
    self.line_pos = line_pos

    return current, old_pos, old_line, old_line_pos
end

-- Test

local function lexer_test()
    local lexer = Lexer.new("1.56 << function main () 2234e2 \n ; -- comment \n   0xFF + +.245 * v0.xyz end ")

    while true do
        local token = Lexer.next(lexer)
        if token == nil then return end
    end
end

-- lexer_test()

return Lexer
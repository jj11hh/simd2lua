local function table_find(t, ele)
    for i = 1,#t do
        if ele == t[i] then return i end
    end

    return nil
end

local function table2sexpr(t)
    if type(t) ~= "table" then return tostring(t) end

    local output = {}
    local lbrace = "{"
    local rbrace = "}"

    for i,v in ipairs(t) do
        output[i] = table2sexpr(v)
    end

    for k,v in pairs(t) do
        if not (type(k) == "number" and k >= 1 and k <= #t) then
            output[#output+1] = k .. "=" .. table2sexpr(v)
        end
    end

    return lbrace .. table.concat(output, " ") .. rbrace
end

return {
    table2sexpr = table2sexpr,
    table_find = table_find,
}


local I = require("inspect")
local T = require("tools")

local lines, count = T.input(4)

--print(I.inspect(lines), count)

local passport_properties = {
        "byr", -- (Birth Year)
        "iyr", -- (Issue Year)
        "eyr", -- (Expiration Year)
        "hgt", -- (Height)
        "hcl", -- (Hair Color)
        "ecl", -- (Eye Color)
        "pid", -- (Passport ID)
        "cid", -- (Country ID)
}

local passports = {}

-- Parse passports
local function parse_lines(lines)
        local p = {}
        for _, line in ipairs(lines) do
                if (# line == 0) then
                        table.insert(passports, p)
                        p = {}
                else
                        for k, v in string.gmatch(line, "(%w+):([#%w]+)") do
                                p[k] = v
                        end
                end
        end
        -- The last one
        if next(p) then
                table.insert(passports, p)
        end
end


local function passport_valid(ppt)
        local valid = true
        for _, k in ipairs(passport_properties) do
                if not ppt[k] and (not (k == "cid")) then
                        --print("Missing", k, I.inspect(ppt))
                        valid = false
                break
            end
        end
        return valid
end

parse_lines(lines)
--print(I.inspect(passports))
print("Passport valid count", T.fold_lines(passports, passport_valid))

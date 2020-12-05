
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
local function parse_lines(plines)
        local p = {}
        for _, line in ipairs(plines) do
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

local function valid_year(year, min, max)
        --print("Year", year, min, max)
        return year and year >= min and year <= max
end

local function valid_height(hgt)
        local hstr = string.sub(hgt, 1, #hgt - 2)
        local unit = string.sub(hgt, #hstr + 1)
        local height = tonumber(hstr)

        --print("Height", height, unit)
        if unit == "cm" then
                return height and height >= 150 and height <= 193
        elseif unit == "in" then
                return height and height >= 59 and height <= 76
        end

        return false
end

local function valid_byte(byte, valid_chars)
        for i = 1, # valid_chars do
            if byte == string.byte(valid_chars, i) then
                return true
            end
        end

        return false
end

local function valid_hair_color(hcl)
        local head = string.sub(hcl, 1, 1)
        local rgb = string.sub(hcl, 2)
        local hex_chars = "0123456789abcdef"

        if (not (head == "#"))
         or (not (# rgb == 6)) then
                return false
        end

        for i = 1, # rgb do
                if not valid_byte(string.byte(rgb, i), hex_chars) then
                        return false
                end
        end

        return true
end

local eye_colors = { "amb", "blu", "brn", "gry", "grn", "hzl", "oth", }

local function valid_eye_color(clr)
        for _, color in ipairs(eye_colors) do
                if clr == color then
                        return true
                end
        end

        return false
end

local function valid_pid(pid)
        local digital_chars = "0123456789"

        if (not (# pid == 9)) then
                return false
        end

        for i = 1, # pid do
                if not valid_byte(string.byte(pid, i), digital_chars) then
                        return false
                end
        end

        return true
end

local passport_properties_validor = {
        -- (Birth Year)
        byr = function (yr) return valid_year(tonumber(yr), 1920, 2002) end,
        -- (Issue Year)
        iyr = function (yr) return valid_year(tonumber(yr), 2010, 2020) end,
        -- (Expiration Year)
        eyr = function (yr) return valid_year(tonumber(yr), 2020, 2030) end,
        -- (Height)
        hgt = valid_height,
        -- (Hair Color)
        hcl = valid_hair_color,
        -- (Eye Color)
        ecl = valid_eye_color,
        -- (Passport ID)
        pid = valid_pid,
        -- (Country ID)
        cid = function ()
            return true
        end,
}

local function passport_valid2(ppt)
        if not passport_valid(ppt) then return false end

        --print("Valid passport again", I.inspect(ppt))
        for _, k in ipairs(passport_properties) do
                local validor = passport_properties_validor[k]
                if not validor(ppt[k]) then
                        --print("Invalid", k, I.inspect(ppt))
                        return false
                end
        end

        return true
end

print("Passport valid2 count", T.fold_lines(passports, passport_valid2))
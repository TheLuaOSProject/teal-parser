local record Point
    x: integer
    y: integer
end

local record MyStruct
    position: Point
    size: Point

    name: string
    age: integer

    is_active: boolean | string
end


local v: MyStruct = {
    position = { x = 10, y = 20 },
    size = { x = 30, y = 40 },

    name = "John Doe",
    age = 25,

    is_active = true
}

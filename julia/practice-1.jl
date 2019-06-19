a = parse(Int, readline())
b, c = parse.(split(readline())) # parse.(Int, split(readline()))
s = chomp(readline())
println(string(a + b + c) * " " * s)

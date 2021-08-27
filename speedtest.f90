program speedtest
    use, intrinsic :: iso_fortran_env
    REAL(real64) :: sum
    INTEGER :: element, iteration, innerloop, array_length, iterations
    REAL(real64), DIMENSION(100000000) :: counter_array
    sum = 0.0
    element = 0
    iteration = 0
    innerloop = 0
    array_length = 100000000
    iterations = 10

    do while (element <= array_length)
        counter_array(element) = REAL(element)
        element = element + 1
    end do
    do while (iteration <= iterations)
        do while (innerloop <= array_length)
            sum = sum + counter_array(Mod((iteration + innerloop), array_length))
            innerloop = innerloop + 1
        end do
        iteration = iteration + 1
    end do
    print *, sum
end program speedtest

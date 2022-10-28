

const infer = .builtin
const type = .builtin
const never = .builtin
const void = .builtin
const unit = .builtin

const array<T, N: usize> = array_mod::array!<T, N>
const bin<N> = bin_mod::bin!<N>
const int<N> = int_mod::int!<N>
const uint<N> = uint_mod::uint!<N>

module global
    const u1 = uint!<1>

module array_mod
    const array<T, N: usize> = .builtin

    fn len<T, N: usize>(_: array<T, N>) usize
        N
    const subscript<T, N> = .builtin

module bin_mod
    const bin<N> = .builtin

    const min<N>: bin<N> = 0
    const max<N> = !min!<N>

    const neg<N> = .builtin
    const add<N> = .builtin
    const sub<N> = .builtin
    const mul<N> = .builtin
    const div<N> = .builtin
    const mod<N> = .builtin

    const eq<N> = .builtin
    const ne<N> = .builtin
    const lt<N> = .builtin
    const gt<N> = .builtin
    const le<N> = .builtin
    const ge<N> = .builtin

    const wide<N> = .builtin
    const bitand<N> = .builtin
    const bitor<N> = .builtin
    const not<N> = .builtin
    const bitxor<N> = .builtin
    const shl<N> = .builtin
    const shr<N> = .builtin

    const brand = .builtin
    const bror = .builtin
    const brnot = .builtin

module int_mod
    const int<N> = .builtin
    
    const min<N>: int<N> = 1 << (N - 1)
    const max<N> = !min!<N>

    const neg<N> = .builtin
    const add<N> = .builtin
    const sub<N> = .builtin
    const mul<N> = .builtin
    const div<N> = .builtin
    const mod<N> = .builtin

    const eq<N> = .builtin
    const ne<N> = .builtin
    const lt<N> = .builtin
    const ge<N> = .builtin
    const le<N> = .builtin
    const gt<N> = .builtin
    //fn ne<N>(left: int<N>, right: int<N>) .b1

    const wide<N> = .builtin
    const shl<N> = .builtin
    const shr<N> = .builtin

module uint_mod
    const uint<N> = .builtin

    const min<N>: uint<N> = 0
    const max<N> = !min!<N>

    const neg<N> = .builtin
    const add<N> = .builtin
    const sub<N> = .builtin
    const mul<N> = .builtin
    const div<N> = .builtin
    const mod<N> = .builtin

    const eq<N> = .builtin
    const ne<N> = .builtin
    const lt<N> = .builtin
    const ge<N> = .builtin
    const le<N> = .builtin
    const gt<N> = .builtin

    const wide<N> = .builtin
    const shl<N> = .builtin
    const shr<N> = .builtin
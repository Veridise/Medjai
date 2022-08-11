%lang starknet

from starkware.cairo.common.cairo_builtins import (HashBuiltin, BitwiseBuiltin)
from starkware.cairo.common.uint256 import (
  Uint256,
  uint256_add,
  uint256_sub,
  uint256_mul,
  uint256_eq,
  uint256_le,
  uint256_check
)
from starkware.cairo.common.math import (
  assert_not_equal,
)

from contracts.vat import (
    Ilk, # structs
    _live, _wards, _can, _ilks, _dai, _Line, _sin, _vice, _debt, # storage vars
    cage, deny, init, hope, nope, file, file_ilk, move, suck, rely# methods
)
from starkware.starknet.common.syscalls import (get_caller_address)
from contracts.safe_math import (
    add, _add, sub, _sub, mul, _mul
)
from contracts.assertions import (
    assert_either, either, both, assert_both,
    not_0, assert_not_0, assert_0, ge,
    ge_0, le, assert_le, le_0, eq_0
)

from contracts.vatspec_helpers import (
    inv_pre,
    inv_post,
    file_failure_cond,
    file_ilk_helper,
    valid_what_ilk,
    verify_eq_uint256,
    verify_le_uint256,
    symbolicUint256,
)

func verify_ilk_eq{range_check_ptr}(a : Ilk, b : Ilk):
    verify_eq_uint256(a.Art, b.Art)
    verify_eq_uint256(a.rate, b.rate)
    verify_eq_uint256(a.spot, b.spot)
    verify_eq_uint256(a.line, b.line)
    verify_eq_uint256(a.dust, b.dust)
    return ()
end

# BEGIN CAGE SPECS
@external
func cage_spec {
        syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr
    }():
   cage()
   let (res) = _live.read()
   verify res = 0
   return ()
end

@external
func cage_inv_test{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}():
    alloc_locals
    local i
    local u

    i = symbolic(felt, 'sym1')
    u = symbolic(felt, 'sym2')

    inv_pre(i, u)
    cage()
    inv_post(i, u)
    return ()
end

@external
func cage_test_revert_necessary{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    ):
    let (caller) = get_caller_address()
    _wards.write(caller, 1)
    cage()
    return ()
end

@external
func cage_test_revert_sufficient{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    ):
    let (caller) = get_caller_address()
    _wards.write(caller, 1)
    cage()
    verify 1 = 0
    return ()
end

# END CAGE SPECS

# BEGIN DENY SPECS

@external
func deny_spec{
    syscall_ptr : felt*,
    pedersen_ptr : HashBuiltin*,
    range_check_ptr
}():
    alloc_locals
    local usr

    usr = symbolic(felt, 'sym')
    deny(usr)
    let (local after) = _wards.read(usr)
    verify after = 0
    return ()
end

# END DENY SPECS

# BEGIN INIT SPECS

@external
func init_spec{
        syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr
    } (ilk: felt):
    alloc_locals
    local what
    local ilk

    what = symbolic(felt, 'sym1')
    ilk = symbolic(felt, 'sym2')
    let (local ilkBefore : Ilk) = _ilks.read(ilk)
    init(ilk)
    let (local ilkAfter : Ilk) = _ilks.read(ilk)
    verify_eq_uint256(ilkBefore.Art, ilkAfter.Art)
    verify_eq_uint256(Uint256(low=10**27, high=0), ilkAfter.rate)
    verify_eq_uint256(ilkBefore.spot, ilkAfter.spot)
    verify_eq_uint256(ilkBefore.line, ilkAfter.line)
    verify_eq_uint256(ilkBefore.dust, ilkAfter.dust)
    return ()
end

# END INIT SPECS

# BEGIN HOPE SPECS

@external
func hope_spec{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}():
    alloc_locals
    local otherFrom
    local otherTo
    local usr

    otherFrom = symbolic(felt, 'otherFrom')  # TODO: fix symbolic to work with compound lhs expressions
    otherTo = symbolic(felt, 'otherTo')
    usr = symbolic(felt, 'usr')
    let (caller) = get_caller_address()  # symbolic(felt, 'caller')
    assert_not_equal(otherFrom, caller)
    assert_not_equal(otherTo, usr)
    let (local canOtherBefore) = _can.read(otherFrom, otherTo)
    hope(usr)
    let (local canAfter) = _can.read(caller, usr)
    let (local canOtherAfter) = _can.read(otherFrom, otherTo)
    verify canAfter = 1
    verify canOtherBefore != canOtherAfter
    return ()
end

# END HOPE SPECS

# BEGIN NOPE SPECS

@external
func nope_spec{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}():
    alloc_locals
    local otherFrom
    local otherTo
    local usr

    otherFrom = symbolic(felt, 'otherFrom')  # TODO: fix symbolic to work with compound lhs expressions
    otherTo = symbolic(felt, 'otherTo')
    usr = symbolic(felt, 'usr')
    let (caller) = get_caller_address()  # symbolic(felt, 'caller')
    assert_not_equal(otherFrom, caller)
    assert_not_equal(otherTo, usr)
    let (local canOtherBefore) = _can.read(otherFrom, otherTo)
    nope(usr)
    let (local canAfter) = _can.read(caller, usr)
    let (local canOtherAfter) = _can.read(otherFrom, otherTo)
    verify canAfter = 0
    verify canOtherBefore = canOtherAfter
    return ()
end

# END NOPE SPECS

# BEGIN MOVE SPECS

@external
func move_demo_spec{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}():
    alloc_locals
    local otherUser
    local src
    local dst

    src = symbolic(felt, 'sym2')
    dst = symbolic(felt, 'sym3')
    assert_not_equal(src, dst)

    # _dai.write(src, Uint256(low=0, high=340282366920938463463374607431768211455))
    let (local daiSrcBefore : Uint256) = _dai.read(src)
    uint256_check(daiSrcBefore)  # Assume the value is a valid uint256
    let (local daiDstBefore : Uint256) = _dai.read(dst)
    uint256_check(daiDstBefore)  # Assume the value is a valid uint256

    let (local rad : Uint256) = symbolicUint256()  # Checked in move
    move(src, dst, rad)

    let (local daiSrcAfter : Uint256) = _dai.read(src)
    let (local daiDstAfter : Uint256) = _dai.read(dst)
    let (res) = uint256_le(daiSrcAfter, daiSrcBefore)
    verify res = 1
    return ()
end

@external
func move_spec{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}():
    alloc_locals
    local other
    local src
    local dst

    src = symbolic(felt, 'sym2')
    dst = symbolic(felt, 'sym3')
    other = symbolic(felt, 'sym4')
    assert_not_equal(src, dst)
    assert_not_equal(src, other)
    assert_not_equal(dst, other)

    let (local daiSrcBefore : Uint256) = _dai.read(src)
    uint256_check(daiSrcBefore)  # Assume the value is a valid uint256
    let (local daiDstBefore : Uint256) = _dai.read(dst)
    uint256_check(daiDstBefore)  # Assume the value is a valid uint256
    let (local daiOtherBefore : Uint256) = _dai.read(other)
    uint256_check(daiOtherBefore)

    let (local rad : Uint256) = symbolicUint256()  # Checked in move
    move(src, dst, rad)

    let (local daiSrcAfter : Uint256) = _dai.read(src)
    let (local daiDstAfter : Uint256) = _dai.read(dst)
    let (local daiOtherAfter : Uint256) = _dai.read(other)
    let (sum : Uint256, _) = uint256_add(daiDstBefore, rad)
    let (diff : Uint256) = uint256_sub(daiSrcBefore, rad)
    verify_eq_uint256(sum, daiDstAfter)
    verify_eq_uint256(diff, daiSrcAfter)
    verify_eq_uint256(daiOtherBefore, daiOtherAfter)
    return ()
end

@external
func move_inv_spec{
    bitwise_ptr : BitwiseBuiltin*, syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr
}():
    alloc_locals
    local i
    local u
    local src
    local dst

    i = symbolic(felt, 'sym1')
    u = symbolic(felt, 'sym2')

    src = symbolic(felt, 'sym3')
    dst = symbolic(felt, 'sym4')
    let (local rad : Uint256) = symbolicUint256()
    uint256_check(rad)

    inv_pre(i, u)
    move(src, dst, rad)
    # _dai.write(src, rad)
    inv_post(i, u)
    return ()
end

# END MOVE SPECS

# BEGIN FILE SPECS

@external
func file_spec{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}():
    alloc_locals
    let what = symbolic(felt, 'sym1')
    let (local data_before : Uint256) = symbolicUint256()
    file(what, data_before)
    let (local data_after : Uint256) = _Line.read()
    verify_eq_uint256(data_before, data_after)
    return ()
end


@external
func file_revert_sufficient_spec{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    ):
    alloc_locals
    local ilk
    local what

    ilk = symbolic(felt, 'ilk')
    what = symbolic(felt, 'what')
    let (res) = file_failure_cond(what)
    assert res = 1
    let (local data) = symbolicUint256()
    let (caller) = get_caller_address()
    file(what, data)
    verify 1 = 0
    return ()
end

@external
func file_revert_necessary_spec{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    ):
    alloc_locals
    local ilk
    local what

    ilk = symbolic(felt, 'ilk')
    what = symbolic(felt, 'what')
    let (local caller) = get_caller_address()
    let (local data) = symbolicUint256()
    _wards.write(caller, 1)
    _live.write(1)
    file('Line', data)
    # Implicitly Medjai checks if a revert happens and returns a failure
    return ()
end

# END FILE SPECS

# BEGIN FILE ILK SPECS

@external
func file_ilk_spec_1{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}():
    alloc_locals
    local what
    local ilk

    what = symbolic(felt, 'sym1')
    ilk = symbolic(felt, 'sym2')
    let (local data : Uint256) = symbolicUint256()
    let (local ilkBefore : Ilk) = _ilks.read(ilk)
    assert what = 'line'
    file_ilk(ilk, what, data)
    let (local ilkAfter : Ilk) = _ilks.read(ilk)
    #verify 1 = 1
    verify_eq_uint256(ilkBefore.Art, ilkAfter.Art)
    verify_eq_uint256(ilkBefore.rate, ilkAfter.rate)
    verify_eq_uint256(ilkBefore.spot, ilkAfter.spot)
    verify_eq_uint256(data, ilkAfter.line)
    verify_eq_uint256(ilkBefore.dust, ilkAfter.dust)
    return ()
end

@external
func file_ilk_spec_2{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}():
    alloc_locals
    local what
    local ilk

    what = symbolic(felt, 'sym1')
    ilk = symbolic(felt, 'sym2')
    let (local data : Uint256) = symbolicUint256()
    let (local ilkBefore : Ilk) = _ilks.read(ilk)
    assert what = 'spot'
    file_ilk(ilk, what, data)
    let (local ilkAfter : Ilk) = _ilks.read(ilk)
    #verify 1 = 1
    verify_eq_uint256(ilkBefore.Art, ilkAfter.Art)
    verify_eq_uint256(ilkBefore.rate, ilkAfter.rate)
    verify_eq_uint256(data, ilkAfter.spot)
    verify_eq_uint256(ilkBefore.line, ilkAfter.line)
    verify_eq_uint256(ilkBefore.dust, ilkAfter.dust)
    return ()
end

@external
func file_ilk_spec_3{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}():
    alloc_locals
    local what
    local ilk

    what = symbolic(felt, 'sym1')
    ilk = symbolic(felt, 'sym2')
    let (local data : Uint256) = symbolicUint256()
    let (local ilkBefore : Ilk) = _ilks.read(ilk)
    assert what = 'dust'
    file_ilk(ilk, what, data)
    let (local ilkAfter : Ilk) = _ilks.read(ilk)
    #verify 1 = 1
    verify_eq_uint256(ilkBefore.Art, ilkAfter.Art)
    verify_eq_uint256(ilkBefore.rate, ilkAfter.rate)
    verify_eq_uint256(ilkBefore.spot, ilkAfter.spot)
    verify_eq_uint256(ilkBefore.line, ilkAfter.line)
    verify_eq_uint256(data, ilkAfter.dust)
    return ()
end

@external
func file_ilk_revert_suff_spec1{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    ):
    # wards = 0, live = 1, what = '(spot' OR 'line' OR 'dust')
    alloc_locals
    local ilk
    local what

    ilk = symbolic(felt, 'ilk')
    what = symbolic(felt, 'what')
    let (local data) = symbolicUint256()
    let (caller) = get_caller_address()
    _wards.write(caller, 0)  # Failure condition
    _live.write(1)  # Set live
    let (res) = file_ilk_helper(what)  # constraint what
    assert res = 1
    file_ilk(ilk, what, data)
    verify 1 = 0
    return ()
end

@external
func file_ilk_revert_suff_spec2{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    ):
    # wards = 1, live = 0, what = ('spot' OR 'line' OR 'dust')
    alloc_locals
    local ilk
    local what

    ilk = symbolic(felt, 'ilk')
    what = symbolic(felt, 'what')
    let (local data) = symbolicUint256()
    let (caller) = get_caller_address()
    _wards.write(caller, 1)  # Set wards
    _live.write(0)  # Failure condition
    let (res) = file_ilk_helper(what)  # constraint what
    assert res = 1
    file_ilk(ilk, what, data)
    verify 1 = 0
    return ()
end

@external
func file_ilk_revert_suff_spec3{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    ):
    # wards = 1, live = 1, what != ('spot' OR 'line' OR 'dust')
    alloc_locals
    local ilk
    local what

    ilk = symbolic(felt, 'ilk')
    what = symbolic(felt, 'what')
    let (local data) = symbolicUint256()
    let (caller) = get_caller_address()
    _wards.write(caller, 1)  # Set what
    _live.write(1)  # Set live
    let (res) = file_ilk_helper(what)  # Failure condition
    assert res = 0
    file_ilk(ilk, what, data)
    verify 1 = 0
    return ()
end

@external
func file_ilk_revert_necessary_spec{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr
}():
    alloc_locals
    local ilk
    local what

    ilk = symbolic(felt, 'ilk')
    what = symbolic(felt, 'what')
    let (local data) = symbolicUint256()
    let (caller) = get_caller_address()
    _wards.write(caller, 1)
    _live.write(1)
    let (res) = file_ilk_helper(what)
    if res == 1:
        file_ilk(ilk, what, data)
        return ()
    else:
        verify 1 = 1
        return ()
    end
end

# END FILE ILK SPECS

# BEGIN SUCK TESTS

@external
func suck_test{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}():
    alloc_locals
    local otherUsrU
    local otherUsrV
    local u
    local v

    otherUsrU = symbolic(felt, 'sym1')
    otherUsrV = symbolic(felt, 'sym2')
    u = symbolic(felt, 'sym3')
    v = symbolic(felt, 'sym4')
    let (local rad : Uint256) = symbolicUint256()

    assert_not_equal(u, otherUsrU)
    assert_not_equal(v, otherUsrV)

    let (sinUBefore) = _sin.read(u)
    uint256_check(sinUBefore)
    let (daiVBefore) = _dai.read(v)
    let (viceBefore) = _vice.read()
    let (debtBefore) = _debt.read()
    let (sinOtherBefore) = _sin.read(otherUsrU)
    let (daiOtherBefore) = _dai.read(otherUsrV)

    suck(u, v, rad)

    let (sinUAfter) = _sin.read(u)
    let (daiVAfter) = _dai.read(v)
    let (viceAfter) = _vice.read()
    let (debtAfter) = _debt.read()
    let (sinOtherAfter) = _sin.read(otherUsrU)
    let (daiOtherAfter) = _dai.read(otherUsrV)

    verify_le_uint256(sinUAfter, sinUBefore)
    verify_le_uint256(daiVBefore, daiVAfter)
    verify_le_uint256(viceBefore, viceAfter)
    verify_le_uint256(debtBefore, debtAfter)
    verify_eq_uint256(sinOtherAfter, sinOtherBefore)
    verify_eq_uint256(daiOtherAfter, daiOtherBefore)
    return ()
end

# END SUCK TESTS

# BEGIN RELY SPEC
@external
func rely_spec{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}():
    alloc_locals
    local usr

    usr = symbolic(felt, 'sym1')
    rely(usr)
    let (local after) = _wards.read(usr)
    verify after = 1
    return ()
end
# END RELY SPEC
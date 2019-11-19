// Copyright 2015 The go-ethereum Authors
// This file is part of the go-ethereum library.
//
// The go-ethereum library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The go-ethereum library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the go-ethereum library. If not, see <http://www.gnu.org/licenses/>.

package vm

import (
	"errors"
	"fmt"
	"github.com/dterei/gotsc"
	"math/big"

	"github.com/syclops/go-ethereum/common"
	"github.com/syclops/go-ethereum/common/math"
	"github.com/syclops/go-ethereum/core/types"
	"github.com/syclops/go-ethereum/params"
	"golang.org/x/crypto/sha3"
)

var (
	bigZero                  = new(big.Int)
	tt255                    = math.BigPow(2, 255)
	errWriteProtection       = errors.New("evm: write protection")
	errReturnDataOutOfBounds = errors.New("evm: return data out of bounds")
	errExecutionReverted     = errors.New("evm: execution reverted")
	errMaxCodeSizeExceeded   = errors.New("evm: max code size exceeded")
	errInvalidJump           = errors.New("evm: invalid jump destination")
)

func opAdd(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	math.U256(y.Add(x, y))

	interpreter.intPool.put(x)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opSub(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	math.U256(y.Sub(x, y))

	interpreter.intPool.put(x)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opMul(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(math.U256(x.Mul(x, y)))

	interpreter.intPool.put(y)

	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opDiv(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if y.Sign() != 0 {
		math.U256(y.Div(x, y))
	} else {
		y.SetUint64(0)
	}
	interpreter.intPool.put(x)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opSdiv(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := math.S256(stack.pop()), math.S256(stack.pop())
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	res := interpreter.intPool.getZero()

	if y.Sign() == 0 || x.Sign() == 0 {
		stack.push(res)
	} else {
		if x.Sign() != y.Sign() {
			res.Div(x.Abs(x), y.Abs(y))
			res.Neg(res)
		} else {
			res.Div(x.Abs(x), y.Abs(y))
		}
		stack.push(math.U256(res))
	}
	interpreter.intPool.put(x, y)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opMod(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if y.Sign() == 0 {
		stack.push(x.SetUint64(0))
	} else {
		stack.push(math.U256(x.Mod(x, y)))
	}
	interpreter.intPool.put(y)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opSmod(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := math.S256(stack.pop()), math.S256(stack.pop())
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	res := interpreter.intPool.getZero()

	if y.Sign() == 0 {
		stack.push(res)
	} else {
		if x.Sign() < 0 {
			res.Mod(x.Abs(x), y.Abs(y))
			res.Neg(res)
		} else {
			res.Mod(x.Abs(x), y.Abs(y))
		}
		stack.push(math.U256(res))
	}
	interpreter.intPool.put(x, y)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opExp(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	base, exponent := stack.pop(), stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	// some shortcuts
	cmpToOne := exponent.Cmp(big1)
	if cmpToOne < 0 { // Exponent is zero
		// x ^ 0 == 1
		stack.push(base.SetUint64(1))
	} else if base.Sign() == 0 {
		// 0 ^ y, if y != 0, == 0
		stack.push(base.SetUint64(0))
	} else if cmpToOne == 0 { // Exponent is one
		// x ^ 1 == x
		stack.push(base)
	} else {
		stack.push(math.Exp(base, exponent))
		interpreter.intPool.put(base)
	}
	interpreter.intPool.put(exponent)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opSignExtend(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	back := stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if back.Cmp(big.NewInt(31)) < 0 {
		bit := uint(back.Uint64()*8 + 7)
		num := stack.pop()
		mask := back.Lsh(common.Big1, bit)
		mask.Sub(mask, common.Big1)
		if num.Bit(int(bit)) > 0 {
			num.Or(num, mask.Not(mask))
		} else {
			num.And(num, mask)
		}

		stack.push(math.U256(num))
	}

	interpreter.intPool.put(back)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opNot(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x := stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	math.U256(x.Not(x))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opLt(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if x.Cmp(y) < 0 {
		y.SetUint64(1)
	} else {
		y.SetUint64(0)
	}
	interpreter.intPool.put(x)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opGt(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if x.Cmp(y) > 0 {
		y.SetUint64(1)
	} else {
		y.SetUint64(0)
	}
	interpreter.intPool.put(x)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opSlt(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()

	xSign := x.Cmp(tt255)
	ySign := y.Cmp(tt255)

	switch {
	case xSign >= 0 && ySign < 0:
		y.SetUint64(1)

	case xSign < 0 && ySign >= 0:
		y.SetUint64(0)

	default:
		if x.Cmp(y) < 0 {
			y.SetUint64(1)
		} else {
			y.SetUint64(0)
		}
	}
	interpreter.intPool.put(x)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opSgt(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()

	xSign := x.Cmp(tt255)
	ySign := y.Cmp(tt255)

	switch {
	case xSign >= 0 && ySign < 0:
		y.SetUint64(0)

	case xSign < 0 && ySign >= 0:
		y.SetUint64(1)

	default:
		if x.Cmp(y) > 0 {
			y.SetUint64(1)
		} else {
			y.SetUint64(0)
		}
	}
	interpreter.intPool.put(x)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opEq(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if x.Cmp(y) == 0 {
		y.SetUint64(1)
	} else {
		y.SetUint64(0)
	}
	interpreter.intPool.put(x)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opIszero(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x := stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if x.Sign() > 0 {
		x.SetUint64(0)
	} else {
		x.SetUint64(1)
	}
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opAnd(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(x.And(x, y))

	interpreter.intPool.put(y)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opOr(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	y.Or(x, y)

	interpreter.intPool.put(x)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opXor(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y := stack.pop(), stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	y.Xor(x, y)

	interpreter.intPool.put(x)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opByte(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	th, val := stack.pop(), stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if th.Cmp(common.Big32) < 0 {
		b := math.Byte(val, 32, int(th.Int64()))
		val.SetUint64(uint64(b))
	} else {
		val.SetUint64(0)
	}
	interpreter.intPool.put(th)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opAddmod(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y, z := stack.pop(), stack.pop(), stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if z.Cmp(bigZero) > 0 {
		x.Add(x, y)
		x.Mod(x, z)
		stack.push(math.U256(x))
	} else {
		stack.push(x.SetUint64(0))
	}
	interpreter.intPool.put(y, z)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opMulmod(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	x, y, z := stack.pop(), stack.pop(), stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if z.Cmp(bigZero) > 0 {
		x.Mul(x, y)
		x.Mod(x, z)
		stack.push(math.U256(x))
	} else {
		stack.push(x.SetUint64(0))
	}
	interpreter.intPool.put(y, z)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

// opSHL implements Shift Left
// The SHL instruction (shift left) pops 2 values from the stack, first arg1 and then arg2,
// and pushes on the stack arg2 shifted to the left by arg1 number of bits.
func opSHL(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	// Note, second operand is left in the stack; accumulate result into it, and no need to push it afterwards
	shift, value := math.U256(stack.pop()), math.U256(stack.peek())
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	defer interpreter.intPool.put(shift) // First operand back into the pool

	if shift.Cmp(common.Big256) >= 0 {
		value.SetUint64(0)
		end := gotsc.BenchEnd()
		opCycles := end - start - tscOverhead
		fmt.Println(contract.GetOp(*pc), opCycles)
		return nil, nil
	}
	n := uint(shift.Uint64())
	math.U256(value.Lsh(value, n))

	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

// opSHR implements Logical Shift Right
// The SHR instruction (logical shift right) pops 2 values from the stack, first arg1 and then arg2,
// and pushes on the stack arg2 shifted to the right by arg1 number of bits with zero fill.
func opSHR(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	// Note, second operand is left in the stack; accumulate result into it, and no need to push it afterwards
	shift, value := math.U256(stack.pop()), math.U256(stack.peek())
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	defer interpreter.intPool.put(shift) // First operand back into the pool

	if shift.Cmp(common.Big256) >= 0 {
		value.SetUint64(0)
		end := gotsc.BenchEnd()
		opCycles := end - start - tscOverhead
		fmt.Println(contract.GetOp(*pc), opCycles)
		return nil, nil
	}
	n := uint(shift.Uint64())
	math.U256(value.Rsh(value, n))

	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

// opSAR implements Arithmetic Shift Right
// The SAR instruction (arithmetic shift right) pops 2 values from the stack, first arg1 and then arg2,
// and pushes on the stack arg2 shifted to the right by arg1 number of bits with sign extension.
func opSAR(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	// Note, S256 returns (potentially) a new bigint, so we're popping, not peeking this one
	shift, value := math.U256(stack.pop()), math.S256(stack.pop())
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	defer interpreter.intPool.put(shift) // First operand back into the pool

	if shift.Cmp(common.Big256) >= 0 {
		if value.Sign() >= 0 {
			value.SetUint64(0)
		} else {
			value.SetInt64(-1)
		}
		stack.push(math.U256(value))
		end := gotsc.BenchEnd()
		opCycles := end - start - tscOverhead
		fmt.Println(contract.GetOp(*pc), opCycles)
		return nil, nil
	}
	n := uint(shift.Uint64())
	value.Rsh(value, n)
	stack.push(math.U256(value))

	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opSha3(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	offset, size := stack.pop(), stack.pop()
	// Note: we start measuring here, but we may want to move this after the memory load.
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	data := memory.Get(offset.Int64(), size.Int64())

	if interpreter.hasher == nil {
		interpreter.hasher = sha3.NewLegacyKeccak256().(keccakState)
	} else {
		interpreter.hasher.Reset()
	}
	interpreter.hasher.Write(data)
	interpreter.hasher.Read(interpreter.hasherBuf[:])

	evm := interpreter.evm
	if evm.vmConfig.EnablePreimageRecording {
		evm.StateDB.AddPreimage(interpreter.hasherBuf, data)
	}
	stack.push(interpreter.intPool.get().SetBytes(interpreter.hasherBuf[:]))

	interpreter.intPool.put(offset, size)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opAddress(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().SetBytes(contract.Address().Bytes()))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opBalance(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	slot := stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	slot.Set(interpreter.evm.StateDB.GetBalance(common.BigToAddress(slot)))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opOrigin(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().SetBytes(interpreter.evm.Origin.Bytes()))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opCaller(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().SetBytes(contract.Caller().Bytes()))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opCallValue(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().Set(contract.value))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opCallDataLoad(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().SetBytes(getDataBig(contract.Input, stack.pop(), big32)))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opCallDataSize(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().SetInt64(int64(len(contract.Input))))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opCallDataCopy(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	var (
		memOffset  = stack.pop()
		dataOffset = stack.pop()
		length     = stack.pop()
	)
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	memory.Set(memOffset.Uint64(), length.Uint64(), getDataBig(contract.Input, dataOffset, length))

	interpreter.intPool.put(memOffset, dataOffset, length)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opReturnDataSize(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().SetUint64(uint64(len(interpreter.returnData))))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opReturnDataCopy(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	var (
		memOffset  = stack.pop()
		dataOffset = stack.pop()
		length     = stack.pop()
	)
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	end := interpreter.intPool.get().Add(dataOffset, length)
	defer interpreter.intPool.put(memOffset, dataOffset, length, end)

	if !end.IsUint64() || uint64(len(interpreter.returnData)) < end.Uint64() {
		benchEnd := gotsc.BenchEnd()
		opCycles := benchEnd - start - tscOverhead
		fmt.Println(contract.GetOp(*pc), opCycles)
		return nil, errReturnDataOutOfBounds
	}
	memory.Set(memOffset.Uint64(), length.Uint64(), interpreter.returnData[dataOffset.Uint64():end.Uint64()])

	benchEnd := gotsc.BenchEnd()
	opCycles := benchEnd - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opExtCodeSize(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	slot := stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	slot.SetUint64(uint64(interpreter.evm.StateDB.GetCodeSize(common.BigToAddress(slot))))

	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opCodeSize(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	l := interpreter.intPool.get().SetInt64(int64(len(contract.Code)))
	stack.push(l)

	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opCodeCopy(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	var (
		memOffset  = stack.pop()
		codeOffset = stack.pop()
		length     = stack.pop()
	)
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	codeCopy := getDataBig(contract.Code, codeOffset, length)
	memory.Set(memOffset.Uint64(), length.Uint64(), codeCopy)

	interpreter.intPool.put(memOffset, codeOffset, length)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opExtCodeCopy(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	var (
		addr       = common.BigToAddress(stack.pop())
		memOffset  = stack.pop()
		codeOffset = stack.pop()
		length     = stack.pop()
	)
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	codeCopy := getDataBig(interpreter.evm.StateDB.GetCode(addr), codeOffset, length)
	memory.Set(memOffset.Uint64(), length.Uint64(), codeCopy)

	interpreter.intPool.put(memOffset, codeOffset, length)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

// opExtCodeHash returns the code hash of a specified account.
// There are several cases when the function is called, while we can relay everything
// to `state.GetCodeHash` function to ensure the correctness.
//   (1) Caller tries to get the code hash of a normal contract account, state
// should return the relative code hash and set it as the result.
//
//   (2) Caller tries to get the code hash of a non-existent account, state should
// return common.Hash{} and zero will be set as the result.
//
//   (3) Caller tries to get the code hash for an account without contract code,
// state should return emptyCodeHash(0xc5d246...) as the result.
//
//   (4) Caller tries to get the code hash of a precompiled account, the result
// should be zero or emptyCodeHash.
//
// It is worth noting that in order to avoid unnecessary create and clean,
// all precompile accounts on mainnet have been transferred 1 wei, so the return
// here should be emptyCodeHash.
// If the precompile account is not transferred any amount on a private or
// customized chain, the return value will be zero.
//
//   (5) Caller tries to get the code hash for an account which is marked as suicided
// in the current transaction, the code hash of this account should be returned.
//
//   (6) Caller tries to get the code hash for an account which is marked as deleted,
// this account should be regarded as a non-existent account and zero should be returned.
func opExtCodeHash(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	slot := stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	address := common.BigToAddress(slot)
	if interpreter.evm.StateDB.Empty(address) {
		slot.SetUint64(0)
	} else {
		slot.SetBytes(interpreter.evm.StateDB.GetCodeHash(address).Bytes())
	}
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opGasprice(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().Set(interpreter.evm.GasPrice))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opBlockhash(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	num := stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()

	n := interpreter.intPool.get().Sub(interpreter.evm.BlockNumber, common.Big257)
	if num.Cmp(n) > 0 && num.Cmp(interpreter.evm.BlockNumber) < 0 {
		stack.push(interpreter.evm.GetHash(num.Uint64()).Big())
	} else {
		stack.push(interpreter.intPool.getZero())
	}
	interpreter.intPool.put(num, n)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opCoinbase(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().SetBytes(interpreter.evm.Coinbase.Bytes()))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opTimestamp(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(math.U256(interpreter.intPool.get().Set(interpreter.evm.Time)))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opNumber(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(math.U256(interpreter.intPool.get().Set(interpreter.evm.BlockNumber)))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opDifficulty(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(math.U256(interpreter.intPool.get().Set(interpreter.evm.Difficulty)))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opGasLimit(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(math.U256(interpreter.intPool.get().SetUint64(interpreter.evm.GasLimit)))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opPop(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	interpreter.intPool.put(stack.pop())
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opMload(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	offset := stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	val := interpreter.intPool.get().SetBytes(memory.Get(offset.Int64(), 32))
	stack.push(val)

	interpreter.intPool.put(offset)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opMstore(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	// pop value of the stack
	mStart, val := stack.pop(), stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	memory.Set32(mStart.Uint64(), val)

	interpreter.intPool.put(mStart, val)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opMstore8(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	off, val := stack.pop().Int64(), stack.pop().Int64()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	memory.store[off] = byte(val & 0xff)

	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opSload(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	loc := stack.peek()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	val := interpreter.evm.StateDB.GetState(contract.Address(), common.BigToHash(loc))
	loc.SetBytes(val.Bytes())
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opSstore(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	loc := common.BigToHash(stack.pop())
	val := stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	interpreter.evm.StateDB.SetState(contract.Address(), loc, common.BigToHash(val))

	interpreter.intPool.put(val)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opJump(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	pos := stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if !contract.validJumpdest(pos) {
		end := gotsc.BenchEnd()
		opCycles := end - start - tscOverhead
		fmt.Println(contract.GetOp(*pc), opCycles)
		return nil, errInvalidJump
	}
	*pc = pos.Uint64()

	interpreter.intPool.put(pos)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opJumpi(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	pos, cond := stack.pop(), stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	if cond.Sign() != 0 {
		if !contract.validJumpdest(pos) {
			end := gotsc.BenchEnd()
			opCycles := end - start - tscOverhead
			fmt.Println(contract.GetOp(*pc), opCycles)
			return nil, errInvalidJump
		}
		*pc = pos.Uint64()
	} else {
		*pc++
	}

	interpreter.intPool.put(pos, cond)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opJumpdest(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opPc(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().SetUint64(*pc))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opMsize(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().SetInt64(int64(memory.Len())))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opGas(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	stack.push(interpreter.intPool.get().SetUint64(contract.Gas))
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opCreate(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	var (
		value        = stack.pop()
		offset, size = stack.pop(), stack.pop()
	)
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	var (
		input        = memory.Get(offset.Int64(), size.Int64())
		gas          = contract.Gas
	)
	if interpreter.evm.chainRules.IsEIP150 {
		gas -= gas / 64
	}

	contract.UseGas(gas)
	res, addr, returnGas, suberr := interpreter.evm.Create(contract, input, gas, value)
	// Push item on the stack based on the returned error. If the ruleset is
	// homestead we must check for CodeStoreOutOfGasError (homestead only
	// rule) and treat as an error, if the ruleset is frontier we must
	// ignore this error and pretend the operation was successful.
	if interpreter.evm.chainRules.IsHomestead && suberr == ErrCodeStoreOutOfGas {
		stack.push(interpreter.intPool.getZero())
	} else if suberr != nil && suberr != ErrCodeStoreOutOfGas {
		stack.push(interpreter.intPool.getZero())
	} else {
		stack.push(interpreter.intPool.get().SetBytes(addr.Bytes()))
	}
	contract.Gas += returnGas
	interpreter.intPool.put(value, offset, size)

	if suberr == errExecutionReverted {
		end := gotsc.BenchEnd()
		opCycles := end - start - tscOverhead
		fmt.Println(contract.GetOp(*pc), opCycles)
		return res, nil
	}
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opCreate2(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	var (
		endowment    = stack.pop()
		offset, size = stack.pop(), stack.pop()
		salt         = stack.pop()
	)
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	var (
		input        = memory.Get(offset.Int64(), size.Int64())
		gas          = contract.Gas
	)

	// Apply EIP150
	gas -= gas / 64
	contract.UseGas(gas)
	res, addr, returnGas, suberr := interpreter.evm.Create2(contract, input, gas, endowment, salt)
	// Push item on the stack based on the returned error.
	if suberr != nil {
		stack.push(interpreter.intPool.getZero())
	} else {
		stack.push(interpreter.intPool.get().SetBytes(addr.Bytes()))
	}
	contract.Gas += returnGas
	interpreter.intPool.put(endowment, offset, size, salt)

	if suberr == errExecutionReverted {
		end := gotsc.BenchEnd()
		opCycles := end - start - tscOverhead
		fmt.Println(contract.GetOp(*pc), opCycles)
		return res, nil
	}
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opCall(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	// Pop gas. The actual gas in interpreter.evm.callGasTemp.
	interpreter.intPool.put(stack.pop())
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	gas := interpreter.evm.callGasTemp
	// Pop other call parameters.
	addr, value, inOffset, inSize, retOffset, retSize := stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop()
	toAddr := common.BigToAddress(addr)
	value = math.U256(value)
	// Get the arguments from the memory.
	args := memory.Get(inOffset.Int64(), inSize.Int64())

	if value.Sign() != 0 {
		gas += params.CallStipend
	}
	ret, returnGas, err := interpreter.evm.Call(contract, toAddr, args, gas, value)
	if err != nil {
		stack.push(interpreter.intPool.getZero())
	} else {
		stack.push(interpreter.intPool.get().SetUint64(1))
	}
	if err == nil || err == errExecutionReverted {
		memory.Set(retOffset.Uint64(), retSize.Uint64(), ret)
	}
	contract.Gas += returnGas

	interpreter.intPool.put(addr, value, inOffset, inSize, retOffset, retSize)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return ret, nil
}

func opCallCode(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	// Pop gas. The actual gas is in interpreter.evm.callGasTemp.
	interpreter.intPool.put(stack.pop())
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	gas := interpreter.evm.callGasTemp
	// Pop other call parameters.
	addr, value, inOffset, inSize, retOffset, retSize := stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop()
	toAddr := common.BigToAddress(addr)
	value = math.U256(value)
	// Get arguments from the memory.
	args := memory.Get(inOffset.Int64(), inSize.Int64())

	if value.Sign() != 0 {
		gas += params.CallStipend
	}
	ret, returnGas, err := interpreter.evm.CallCode(contract, toAddr, args, gas, value)
	if err != nil {
		stack.push(interpreter.intPool.getZero())
	} else {
		stack.push(interpreter.intPool.get().SetUint64(1))
	}
	if err == nil || err == errExecutionReverted {
		memory.Set(retOffset.Uint64(), retSize.Uint64(), ret)
	}
	contract.Gas += returnGas

	interpreter.intPool.put(addr, value, inOffset, inSize, retOffset, retSize)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return ret, nil
}

func opDelegateCall(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	// Pop gas. The actual gas is in interpreter.evm.callGasTemp.
	interpreter.intPool.put(stack.pop())
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	gas := interpreter.evm.callGasTemp
	// Pop other call parameters.
	addr, inOffset, inSize, retOffset, retSize := stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop()
	toAddr := common.BigToAddress(addr)
	// Get arguments from the memory.
	args := memory.Get(inOffset.Int64(), inSize.Int64())

	ret, returnGas, err := interpreter.evm.DelegateCall(contract, toAddr, args, gas)
	if err != nil {
		stack.push(interpreter.intPool.getZero())
	} else {
		stack.push(interpreter.intPool.get().SetUint64(1))
	}
	if err == nil || err == errExecutionReverted {
		memory.Set(retOffset.Uint64(), retSize.Uint64(), ret)
	}
	contract.Gas += returnGas

	interpreter.intPool.put(addr, inOffset, inSize, retOffset, retSize)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return ret, nil
}

func opStaticCall(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	// Pop gas. The actual gas is in interpreter.evm.callGasTemp.
	interpreter.intPool.put(stack.pop())
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	gas := interpreter.evm.callGasTemp
	// Pop other call parameters.
	addr, inOffset, inSize, retOffset, retSize := stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop()
	toAddr := common.BigToAddress(addr)
	// Get arguments from the memory.
	args := memory.Get(inOffset.Int64(), inSize.Int64())

	ret, returnGas, err := interpreter.evm.StaticCall(contract, toAddr, args, gas)
	if err != nil {
		stack.push(interpreter.intPool.getZero())
	} else {
		stack.push(interpreter.intPool.get().SetUint64(1))
	}
	if err == nil || err == errExecutionReverted {
		memory.Set(retOffset.Uint64(), retSize.Uint64(), ret)
	}
	contract.Gas += returnGas

	interpreter.intPool.put(addr, inOffset, inSize, retOffset, retSize)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return ret, nil
}

func opReturn(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	offset, size := stack.pop(), stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	ret := memory.GetPtr(offset.Int64(), size.Int64())

	interpreter.intPool.put(offset, size)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return ret, nil
}

func opRevert(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	offset, size := stack.pop(), stack.pop()
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	ret := memory.GetPtr(offset.Int64(), size.Int64())

	interpreter.intPool.put(offset, size)
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return ret, nil
}

func opStop(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

func opSuicide(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	balance := interpreter.evm.StateDB.GetBalance(contract.Address())
	interpreter.evm.StateDB.AddBalance(common.BigToAddress(stack.pop()), balance)

	interpreter.evm.StateDB.Suicide(contract.Address())
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

// following functions are used by the instruction jump  table

// make log instruction function
func makeLog(size int) executionFunc {
	return func(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
		topics := make([]common.Hash, size)
		mStart, mSize := stack.pop(), stack.pop()
		for i := 0; i < size; i++ {
			topics[i] = common.BigToHash(stack.pop())
		}

		d := memory.Get(mStart.Int64(), mSize.Int64())
		interpreter.evm.StateDB.AddLog(&types.Log{
			Address: contract.Address(),
			Topics:  topics,
			Data:    d,
			// This is a non-consensus field, but assigned here because
			// core/state doesn't know the current block number.
			BlockNumber: interpreter.evm.BlockNumber.Uint64(),
		})

		interpreter.intPool.put(mStart, mSize)
		return nil, nil
	}
}

// opPush1 is a specialized version of pushN
func opPush1(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
	tscOverhead := gotsc.TSCOverhead()
	start := gotsc.BenchStart()
	var (
		codeLen = uint64(len(contract.Code))
		integer = interpreter.intPool.get()
	)
	*pc += 1
	if *pc < codeLen {
		stack.push(integer.SetUint64(uint64(contract.Code[*pc])))
	} else {
		stack.push(integer.SetUint64(0))
	}
	end := gotsc.BenchEnd()
	opCycles := end - start - tscOverhead
	fmt.Println(contract.GetOp(*pc), opCycles)
	return nil, nil
}

// make push instruction function
func makePush(size uint64, pushByteSize int) executionFunc {
	return func(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
		codeLen := len(contract.Code)

		startMin := codeLen
		if int(*pc+1) < startMin {
			startMin = int(*pc + 1)
		}

		endMin := codeLen
		if startMin+pushByteSize < endMin {
			endMin = startMin + pushByteSize
		}

		integer := interpreter.intPool.get()
		stack.push(integer.SetBytes(common.RightPadBytes(contract.Code[startMin:endMin], pushByteSize)))

		*pc += size
		return nil, nil
	}
}

// make dup instruction function
func makeDup(size int64) executionFunc {
	return func(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
		stack.dup(interpreter.intPool, int(size))
		return nil, nil
	}
}

// make swap instruction function
func makeSwap(size int64) executionFunc {
	// switch n + 1 otherwise n would be swapped with n
	size++
	return func(pc *uint64, interpreter *EVMInterpreter, contract *Contract, memory *Memory, stack *Stack) ([]byte, error) {
		stack.swap(int(size))
		return nil, nil
	}
}

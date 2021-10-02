# 6502-emu
This is a Python module that provides an emulated MOS6502 CPU.

**Warning: this does not execute illegal instructions!**

**Warning: accessing undocumented features is unsupported!**
# `mos6502.py`
This file provides the base module, which contains the `MOS6502` class.
## `MOS6502(memRead, memWrite, *, A = 0, X = 0, Y = 0, C = 0, Z = 1, B = 1, V = 0, N = 0, m = 1)`
Initializes a CPU attached to a device described by `memRead` and `memWrite`.

+ `memRead(adr)` will be called whenever the CPU reads the byte at address `adr` and must return an integer, the lowest 8 bits of which shall be the read byte
+ `memWrite(adr, val)` will be called whenever the CPU writes byte `val` to address `adr`

`adr` is guaranteed to be within `0x0000` and `0xffff`, and `val` is guaranteed to be within `0x00` and `0xff`. Whenever a jump vector is read, the low byte will be read before the high byte.

Note that there are no additional requirements to these functions, which means that they are the gateway to implementing any (emulated) system that uses the 6502.

The other arguments are passed to [`MOS6502.RES`][res] after storing the device info.
## `MOS6502.NMI()`
Sends a non-maskable interrupt request.
## `MOS6502.RES(*, A = None, X = None, Y = None, C = None, Z = None, B = None, V = None, N = None, m = None)`
Sends a reset signal. The optional arguments provide initial values to some registers, as follows:
Argument|Bit length|Register
:-:|:-:|:-:
`A`|8|**A**
`X`|8|**X**
`Y`|8|**Y**
`C`|1|**P & 01h** (carry flag)
`Z`|1|**P & 02h** (zero flag)
`B`|1|**P & 10h** (break flag)
`V`|1|**P & 40h** (overflow flag)
`N`|1|**P & 80h** (negative flag)
`m`|1|**P & 20h** (unused flag)

The values are truncated to as many lowest bits as indicated in the table, and are also stored internally within the `MOS6502` instance, and are reused if a subsequent call to [`MOS6502.RES`][res] omits them.
## `MOS6502.IRQ()`
Sends a maskable interrupt request.
## `MOS6502.step()`
Executes an instruction and returns it as a list of bytes (`list[int]`).
## Register values
The following attributes of a `MOS6502` instance reveal the contents of its registers:
Attribute|Register
:-:|:-:
`MOS6502.PC`|**PC**
`MOS6502.A`|**A**
`MOS6502.X`|**X**
`MOS6502.Y`|**Y**
`MOS6502.S`|**S**
`MOS6502.P`|**P**
`MOS6502.C`|**P & 01h** (carry flag)
`MOS6502.Z`|**P & 02h** (zero flag)
`MOS6502.I`|**P & 04h** (interrupt flag)
`MOS6502.D`|**P & 08h** (decimal flag)
`MOS6502.B`|**P & 10h** (break flag)
`MOS6502.m`|**P & 20h** (unused flag)
`MOS6502.V`|**P & 40h** (overflow flag)
`MOS6502.N`|**P & 80h** (negative flag)

[res]: #mos6502res-a--none-x--none-y--none-c--none-z--none-b--none-v--none-n--none-m--none
# `basic6502device.py`
This is an example implementation of a simple computer with STDIN and STDOUT streams. Reading from **00ffh** reads a byte off of the input stream, and writing to **00ffh** writes a byte to the output stream.

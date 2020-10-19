using System;

namespace NESEmulator
{
    class Bus
    {

        Rain6502 cpu = new Rain6502();
        public byte[] ram = new byte[65536];

        public Bus()
        {
            Array.ForEach(ram, x => x = 0x00);

            cpu.ConnectBus(this);
        }

        public void Write(ushort addr, byte data)
        {
            if (addr >= 0x0000 && addr <= 0xFFFF)
                ram[addr] = data;
            else
                throw new Exception($"Out of range address : {addr}");
        }

        public byte Read(ushort addr, bool readOnly = false)
        {
            if (addr >= 0x0000 && addr <= 0xFFFF)
                return ram[addr];
            else
                return 0x00;
        }
    }
}

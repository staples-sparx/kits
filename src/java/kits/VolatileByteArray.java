package kits;

import java.util.Arrays;

public class VolatileByteArray {
    private volatile int whichBuffer;
    private byte[][] buffers;

    public VolatileByteArray(int bufferSize) {
        this.whichBuffer = 0;
        this.buffers = new byte[2][bufferSize]; 
    }

    private byte[] otherBuffer() {
        return buffers[1 ^ whichBuffer];
    }

    private byte[] currentBuffer() {
        return buffers[whichBuffer];
    }

    public byte[] put(byte[] src) {
        System.arraycopy(src, 0, otherBuffer(), 0, src.length);
        whichBuffer ^= 1;
        return currentBuffer();
    }

    public byte[] get() {
        return currentBuffer();
    }

    public void clear() {
        Arrays.fill(otherBuffer(), (byte) 0);
        whichBuffer ^= 1;
    }
}

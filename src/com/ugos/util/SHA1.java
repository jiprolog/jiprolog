
package com.ugos.util;


/**
 * @author  Ugo Chirico <ugo.chirico@ugosweb.com>
 * Customized and ported on J2ME
 */
public final class SHA1
{
   //  Length of the final hash (in bytes).
   private static final int HASH_LENGTH = 20;

   //  Length of a block (i.e. the number of bytes hashed in every transform).
   private static final int DATA_LENGTH = 64;

   //  The buffer used to store the last incomplete block.
   private byte[] buffer;

   //  The number of bytes currently stored in buffer[].
   private int buffered;

   //  The number of bytes that have been input to the digest.
   private long count;

   private int[] digest;
   private int[] data;
   private int[] z;
   private byte[] tmp;


   /**
    * Constructs a SHA-1 message digest.
    */
  public  SHA1()
   {
      buffer = new byte[DATA_LENGTH];
      digest = new int[HASH_LENGTH/4];
      data = new int[DATA_LENGTH/4];
      tmp = new byte[DATA_LENGTH];
      z = new int[80];
      engineReset();
   }

  /**
   *  This method accepts a string and returns a SHA-1 secure
   *  one-way hash value of that string, as a byte array.
   */
  public static byte[] hash(byte[] input)
  {
      SHA1 sha1 = new SHA1();
      return sha1.getSHA1Hash(input);
  }

  public static byte[] hash(String str)
  {
     return hash(str.getBytes());
  }

   /**
    *  This method accepts a string and returns a SHA-1 secure
    *  one-way hash value of that string, as a byte array.
    */
  public byte[] getSHA1Hash(String str)
   {
      return getSHA1Hash(str.getBytes());
   }


   /**
    *  This method accepts a byte array and returns a SHA-1 secure
    *  one-way hash value of it, as a byte array.
    */
   public  byte[] getSHA1Hash(byte[] input)
   {
      engineReset();
      engineUpdate(input,0,input.length);
      return engineDigest();
   }

  	public void update(byte[] input)
  	{
  	    engineUpdate(input,0,input.length);
  	}

  	public void reset()
  	{
  	    engineReset();
  	}

  	public byte[] digest()
  	{
  	    return engineDigest();
  	}

   /**
    * Initializes (resets) the message digest.
    */
   private void engineReset()
   {
      buffered = 0;
      count = 0;
      digest[0] = 0x67452301;
      digest[1] = 0xefcdab89;
      digest[2] = 0x98badcfe;
      digest[3] = 0x10325476;
      digest[4] = 0xc3d2e1f0;

      // A little clean up...
      for (int i=0; i < DATA_LENGTH; i++)
         {  tmp[i] = 0;  buffer[i] = 0;  }
      for (int i=0; i < DATA_LENGTH/4; i++)
         data[i] = 0;
      for (int i=0; i < 80; i++)
         z[i] = 0;
   }


   /**
    *  Updates the message digest with a single byte of new data.
    */
   void engineUpdate(byte b)
   {
      byte[] data = { b };
      engineUpdate(data, 0, 1);
   }


   /**
    *  Updates the message digest with new data.
    *
    * @param data      the data to be added.
    * @param offset    the start of the data in the array.
    * @param length    the number of bytes of data to add.
    */
   void engineUpdate(byte[] data, int offset, int length)
   {
      count += length;

      int datalen = DATA_LENGTH;
      int remainder;

      //  Transform whole blocks (of size DATA_LENGTH).
      while (length >= (remainder = datalen - buffered)) {
         System.arraycopy(data, offset, buffer, buffered, remainder);
         engineTransform(buffer);
         length -= remainder;
         offset += remainder;
         buffered = 0;
      }

      //  Add any remaining data (non-whole block) to buffer.
      if (length > 0)
      {
         System.arraycopy(data, offset, buffer, buffered, length);
         buffered += length;
      }
   }


   /**
    *  Calculates the final digest.
    */
   byte[] engineDigest()
   {
      return engineDigest(buffer, buffered);
   }


   /**
    * Returns the digest of all data added (previous and buffered) and resets the digest.
    * @return    the digest of all the data added to the message digest as a byte array.
    */
   private byte[] engineDigest(byte[] in, int pos)
   {
      if (pos != 0) System.arraycopy(in, 0, tmp, 0, pos);

      tmp[pos++] = (byte)0x80;

      if (pos > DATA_LENGTH - 8)
      {
         while (pos < DATA_LENGTH)
            tmp[pos++] = 0;

         byte2int(tmp, 0, data, 0, DATA_LENGTH/4);
         transform(data);
         pos = 0;
      }

      while (pos < DATA_LENGTH - 8)
         tmp[pos++] = 0;

      byte2int(tmp, 0, data, 0, (DATA_LENGTH/4)-2);

      // Big endian
      // WARNING: int>>>32 != 0 !!!
      long bc = count * 8;
      data[14] = (int) (bc>>>32);
      data[15] = (int) bc;

      transform(data);

      byte buf[] = new byte[HASH_LENGTH];

      // Big endian
      int off = 0;
      for (int i = 0; i < HASH_LENGTH/4; ++i) {
         int d = digest[i];
         buf[off++] = (byte) (d>>>24);
         buf[off++] = (byte) (d>>>16);
         buf[off++] = (byte) (d>>>8);
         buf[off++] = (byte)  d;
      }

      engineReset();
      return buf;
   }


   /**
    *  Transform (add) a data block to the message digest.
    */
   private void engineTransform(byte[] in)
   {
      byte2int(in, 0, data, 0, DATA_LENGTH/4);
      transform(data);
   }


   // Helper function.  Note: parameter order consistent with System.arraycopy.
   private static void byte2int(byte[] src, int srcOffset,
                                int[] dst, int dstOffset, int length)
   {
      while (length-- > 0)
      {
         // Big endian
         dst[dstOffset++] = ( src[srcOffset++]         << 24) |
                            ((src[srcOffset++] & 0xFF) << 16) |
                            ((src[srcOffset++] & 0xFF) <<  8) |
                             (src[srcOffset++] & 0xFF);
      }
   }


// SHA-1 transform routines
// -----------------------------------------------------------------------------------

   private static int f1(int a, int b, int c) { return (c^(a&(b^c))) + 0x5A827999; }
   private static int f2(int a, int b, int c) { return (a^b^c) + 0x6ED9EBA1; }
   private static int f3(int a, int b, int c) { return ((a&b)|(c&(a|b))) + 0x8F1BBCDC; }
   private static int f4(int a, int b, int c) { return (a^b^c) + 0xCA62C1D6; }

   private void transform (int[] X)
   {
      int A = digest[0];
      int B = digest[1];
      int C = digest[2];
      int D = digest[3];
      int E = digest[4];

      int W[] = z;
      for (int i=0; i<16; i++)
         W[i] = X[i];

      for (int i=16; i<80; i++)
      {
         int j = W[i-16] ^ W[i-14] ^ W[i-8] ^ W[i-3];
         W[i] = j;
         W[i] = (j << 1) | (j >>> -1);
      }

      E += ((A << 5)|(A >>> -5)) + f1(B, C, D) + W[0];  B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f1(A, B, C) + W[1];  A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f1(E, A, B) + W[2];  E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f1(D, E, A) + W[3];  D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f1(C, D, E) + W[4];  C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f1(B, C, D) + W[5];  B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f1(A, B, C) + W[6];  A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f1(E, A, B) + W[7];  E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f1(D, E, A) + W[8];  D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f1(C, D, E) + W[9];  C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f1(B, C, D) + W[10]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f1(A, B, C) + W[11]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f1(E, A, B) + W[12]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f1(D, E, A) + W[13]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f1(C, D, E) + W[14]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f1(B, C, D) + W[15]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f1(A, B, C) + W[16]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f1(E, A, B) + W[17]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f1(D, E, A) + W[18]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f1(C, D, E) + W[19]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f2(B, C, D) + W[20]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f2(A, B, C) + W[21]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f2(E, A, B) + W[22]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f2(D, E, A) + W[23]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f2(C, D, E) + W[24]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f2(B, C, D) + W[25]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f2(A, B, C) + W[26]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f2(E, A, B) + W[27]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f2(D, E, A) + W[28]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f2(C, D, E) + W[29]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f2(B, C, D) + W[30]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f2(A, B, C) + W[31]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f2(E, A, B) + W[32]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f2(D, E, A) + W[33]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f2(C, D, E) + W[34]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f2(B, C, D) + W[35]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f2(A, B, C) + W[36]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f2(E, A, B) + W[37]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f2(D, E, A) + W[38]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f2(C, D, E) + W[39]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f3(B, C, D) + W[40]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f3(A, B, C) + W[41]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f3(E, A, B) + W[42]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f3(D, E, A) + W[43]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f3(C, D, E) + W[44]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f3(B, C, D) + W[45]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f3(A, B, C) + W[46]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f3(E, A, B) + W[47]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f3(D, E, A) + W[48]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f3(C, D, E) + W[49]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f3(B, C, D) + W[50]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f3(A, B, C) + W[51]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f3(E, A, B) + W[52]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f3(D, E, A) + W[53]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f3(C, D, E) + W[54]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f3(B, C, D) + W[55]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f3(A, B, C) + W[56]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f3(E, A, B) + W[57]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f3(D, E, A) + W[58]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f3(C, D, E) + W[59]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f4(B, C, D) + W[60]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f4(A, B, C) + W[61]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f4(E, A, B) + W[62]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f4(D, E, A) + W[63]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f4(C, D, E) + W[64]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f4(B, C, D) + W[65]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f4(A, B, C) + W[66]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f4(E, A, B) + W[67]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f4(D, E, A) + W[68]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f4(C, D, E) + W[69]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f4(B, C, D) + W[70]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f4(A, B, C) + W[71]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f4(E, A, B) + W[72]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f4(D, E, A) + W[73]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f4(C, D, E) + W[74]; C =((C << 30)|(C >>> -30));
      E += ((A << 5)|(A >>> -5)) + f4(B, C, D) + W[75]; B =((B << 30)|(B >>> -30));
      D += ((E << 5)|(E >>> -5)) + f4(A, B, C) + W[76]; A =((A << 30)|(A >>> -30));
      C += ((D << 5)|(D >>> -5)) + f4(E, A, B) + W[77]; E =((E << 30)|(E >>> -30));
      B += ((C << 5)|(C >>> -5)) + f4(D, E, A) + W[78]; D =((D << 30)|(D >>> -30));
      A += ((B << 5)|(B >>> -5)) + f4(C, D, E) + W[79]; C =((C << 30)|(C >>> -30));

      digest[0] += A;
      digest[1] += B;
      digest[2] += C;
      digest[3] += D;
      digest[4] += E;
   }

}  // end SHA1

/*
 *   File:  Conversions.java
 *
 *   Misc. data conversion routines.
 *
 *   version 1.032 v1a
 *   Copyright 1998, 1999 by Hush Communications Corporation, BWI
 */

package com.ugos.util;

import java.math.BigInteger;


public final class Encoder
{
//   public static String bytesToHexString(byte[] b)
//   {
//      String outputString = "";
//      /*  Create a byte array with an extra byte with MSB 0
//       *  so BigInteger will return a positive number
//       */
//      byte[] bb = new byte[b.length+1];
//      bb[0] = 127;
//      System.arraycopy(b,0,bb,1,b.length);
//
//      /*  Create a BigInteger from the byte array, convert to a string
//       *  and chop off the first two characters, which represent
//       *  the previously added byte
//       */
//      outputString = new BigInteger(bb).toString(16).substring(2);
//      return outputString.toUpperCase();
//   }
//
//
//  /**
//   *  This method accepts a hex string and returns a byte array.
//   *  The string must represent an integer number of bytes.
//   */
//   public static byte[] hexStringToBytes(String hex)
//   {
//      byte[] bigIntBytes = new BigInteger(hex,16).toByteArray();
//      if (bigIntBytes.length > hex.length()/2)
//      {
//         byte[] outbytes = new byte[hex.length()/2];
//         System.arraycopy(bigIntBytes, bigIntBytes.length-outbytes.length,
//            outbytes, 0, outbytes.length);
//         return outbytes;
//      }
//      else if (bigIntBytes.length < hex.length()/2)
//      {
//         byte[] outbytes = new byte[hex.length()/2];
//         System.arraycopy(bigIntBytes, 0,
//            outbytes, outbytes.length - bigIntBytes.length, bigIntBytes.length);
//         return outbytes;
//      }
//      else return bigIntBytes;
//   }


   /*  this function accepts 8 bytes and returns a long integer
    */
    public static long bytesToLong(byte[] bytes)
    {
        //boolean negative = false;
        long returnLong = 0;
        for (int n=0; n<8; n++)
        {
         returnLong = returnLong << 8;
         long aByte = bytes[n];
         if (aByte<0) aByte = aByte + 256;
         returnLong = returnLong | aByte;
        }
        return returnLong;
    }


    public static byte[] longToBytes(long l)
    {
      long ll = l;
      long mask = 255;
      byte[] returnBytes = new byte[8];
      long temp = 0;
      for (int n=7; n>=0; n--)
      {
         temp = ll & mask;
         if (temp > 127) temp = temp - 256;
         returnBytes[n] = (byte)ll;
         ll = ll >>> 8;
      }
      return returnBytes;
    }


   /*  this function accepts 8 bytes and returns a long integer
    */
    public static int bytesToInt(byte[] bytes)
    {
     // boolean negative = false;
      int returnInt = 0;
      for (int n=0; n<4; n++)
      {
         returnInt = returnInt << 8;
         int aByte = bytes[n];
         if (aByte<0) aByte = aByte + 256;
         returnInt = returnInt | aByte;
      }
      return returnInt;
    }


    public static byte[] intToBytes(int i)
    {
      int ii = i;
      int mask = 255;
      byte[] returnBytes = new byte[4];
      int temp = 0;
      for (int n=3; n>=0; n--)
      {
         temp = ii & mask;
         if (temp > 127) temp = temp - 256;
         returnBytes[n] = (byte)ii;
         ii = ii >>> 8;
      }
      return returnBytes;
    }
    
    /** The hexadecimal digits "0" through "f". */
    private static char[] NIBBLE = {
                                      '0', '1', '2', '3', '4', '5', '6', '7',
                                      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
                                  };


    /**
     * Convert a byte array to a string of hexadecimal digits.
     */
    public static final String bytesToHexString(byte[] buf)
    {
        return bytesToHexString(buf, 0, buf.length);
    }


    /**
     * Convert a byte array to a string of hexadecimal digits.
     * The bytes <code>buf[i..i+length-1]</code> are used.
     */
    public static final String bytesToHexString(byte[] buf, int i, int length)
    {
        StringBuffer sb = new StringBuffer(length*2);
        for (int j=i; j<i+length; j++) {
            sb.append(NIBBLE[(buf[j]>>>4)&15]);
            sb.append(NIBBLE[ buf[j]     &15]);
        }
        return sb.toString().toUpperCase();
    }


    /**
     * Convert a long to a string of hexadecimal digits.
     */
    public static final String longToHexString(long a)
    {
        StringBuffer sb = new StringBuffer(16);
        for (int i=0; i<16; i++)
            sb.append(NIBBLE[(int)(a >>> (60-4*i)) & 0xf]);
        return sb.toString();
    }


    /**
     * Convert an int to a string of hexadecimal digits.
     */
    public static final String intToHexString(int a)
    {
        StringBuffer sb = new StringBuffer(8);
        for (int i=0; i<8; i++)
            sb.append(NIBBLE[(a >>> (60-4*i)) & 0xf]);
        return sb.toString();
    }


    /**
     * Convert a byte to a string of hexadecimal digits.
     */
    public static final String byteToHexString(byte a)
    {
        StringBuffer sb = new StringBuffer(2);
        sb.append(NIBBLE[(a>>>4)&0xf]);
        sb.append(NIBBLE[a&0xf]);
        return sb.toString();
    }


    /**
     * Convert a hexadecimal digit to a byte.
     */
    public static byte hexDigitToByte(char hDigit)
    {
        if(hDigit <= '9')
            return (byte)(hDigit-'0');
        if(hDigit <= 'G')
            return (byte)(hDigit-('A'-10));
        return (byte)(hDigit-('a'-10));
    }


    /**
     * Convert a string of hexadecimal digits to a byte array.
     */
    public static byte[] hexStringToBytes(String hex)
    {
        int l=(hex.length()+1)/2;
        byte[] r = new byte[l];
        int i = 0;
        int j = 0;
        if(hex.length()%2 == 1) {
            // Odd number of characters: must handle half byte first.
            r[0]=hexDigitToByte(hex.charAt(0));
            i=j=1;
        }
        while(i<l)
            r[i++] = (byte)((hexDigitToByte(hex.charAt(j++)) << 4) | hexDigitToByte(hex.charAt(j++)));
        return r;
    }


    /** The binary digits "0" and "1". */
    private static char[] BIT = { '0', '1' };


    /**
     * Convert a long to a string of binary digits.
     */
    public static final String longToBinString(long a)
    {
        StringBuffer sb = new StringBuffer(64);
        for (int i=0; i<64; i++)
            sb.append(BIT[(int)(a >>> (63-i)) & 0x1]);
        return sb.toString();
    }


    /**
     * Convert an int to a string of binary digits.
     */
    public static final String longToBinString(int a)
    {
        StringBuffer sb = new StringBuffer(32);
        for (int i=0; i<32; i++)
            sb.append(BIT[(a >>> (31-i)) & 0x1]);
        return sb.toString();
    }


}  //  end Conversions

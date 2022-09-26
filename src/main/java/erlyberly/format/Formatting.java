/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 * <p>
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly.format;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangBitstr;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Utility methods for formatting.
 */
enum Formatting {
    ;

    static void appendString(final OtpErlangString aString, final TermFormatter formatter, final String quote, final StringBuilder sb) {
        final String stringValue = aString.stringValue();
        // sometimes a list of integers can be mis-typed by jinterface as a string
        // in this case we format it ourselves as a list of ints
        final boolean isString = isString(stringValue);
        if (isString) {
            sb.append(quote).append(stringValue.replace("\n", "\\n")).append(quote);
        } else {
            appendListOfInts(stringValue, formatter, sb);
        }
    }

    private static void appendListOfInts(final String stringValue, final TermFormatter formatter, final StringBuilder sb) {
        sb.append(formatter.listLeftParen());
        for (int i = 0; i < stringValue.length(); i++) {
            final int c = stringValue.charAt(i);
            sb.append(c);
            if (i < (stringValue.length() - 1)) {
                sb.append(",");
            }
        }
        sb.append(formatter.listRightParen());
    }

    private static boolean isString(final String stringValue) {
        final int length = stringValue.length();
        for (int i = 0; i < length; i++) {
            final char c = stringValue.charAt(i);
            if (10 > c || 127 < c) {
                return false;
            }
        }
        return true;
    }

    /**
     * Append the binary term bytes to a string builder. It attempts to display character data
     * as strings.
     */
    static void binaryToString(final OtpErlangBinary bin, final String sep, final StringBuilder sb) {
        final int length = sb.length();
        boolean inString = false;

        for (int b : bin.binaryValue()) {
            if (isDisplayableChar(b)) {
                if (!inString) {
                    if (0 < (sb.length() - length)) {
                        sb.append(sep);
                    }

                    sb.append("\"");
                }
                inString = true;
                sb.append((char) b);
            } else {
                if (inString) {
                    sb.append("\"");
                    inString = false;
                }

                if (0 < (sb.length() - length)) {
                    sb.append(sep);
                }

                if (0 > b) {
                    b = 256 + b;
                }
                sb.append(b);
            }
        }

        if (inString) {
            sb.append("\"");
        }
    }

    static void bitstringToString(final OtpErlangBitstr bits, final String sep,
                                  final String bitsFormat, final StringBuilder sb) {
        final byte[] binValue = bits.binaryValue();
        for (int i = 0; i < bits.size(); i++) {
            final int b = 0 <= binValue[i] ? binValue[i] : binValue[i] + 256;
            sb.append(String.format("%d%s", b, sep));
        }
        int b = binValue[bits.size()];
        b = (0 <= b ? b : b + 256) >> bits.pad_bits();
        sb.append(String.format(bitsFormat, b, 8 - bits.pad_bits()));
    }

    private static boolean isDisplayableChar(final int b) {
        return 31 < b && 127 > b;
    }

    /**
     * Make a guess if the given binary is a UTF-8 string or not.
     */
    static boolean isDisplayableString(final OtpErlangBinary bin) {
        int expected;
        final byte[] bytes = bin.binaryValue();
        for (int i = 0; i < bytes.length; i++) {
            final int ch = bytes[i];
            if (isDisplayableChar(ch)) continue;
            else if (0b11000000 == (ch & 0b11100000)) expected = 1;
            else if (0b11100000 == (ch & 0b11110000)) expected = 2;
            else if (0b11100000 == (ch & 0b11111000)) expected = 3;
            else return false;

            while (0 < expected) {
                if (i + expected >= bytes.length) return false;
                if (0b10000000 != (bytes[i + 1] & 0b11000000)) return false;
                expected--;
                i++;
            }
        }
        return true;
    }
}

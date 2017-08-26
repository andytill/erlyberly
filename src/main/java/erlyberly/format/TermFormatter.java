/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly.format;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Formatter API for erlang terms, functions, exceptions, and everything
 * else which differs per language.
 */
public interface TermFormatter {

    String modFuncArgsToString(OtpErlangTuple mfa);

    /**
     * For example mymod:myfunc/2 in an erlang implementation.
     */
    String modFuncArityToString(OtpErlangTuple mfa);

    default String modFuncArityToString(String mod, String func, int arity) { return mod + ":" + func + "/" + arity; }

    default String moduleNameToString(String modName) { return modName; }

    String exceptionToString(OtpErlangAtom errorClass, OtpErlangObject errorReason);

    StringBuilder appendToString(OtpErlangObject obj, StringBuilder sb);

    default String toString(OtpErlangObject obj) {
        return appendToString(obj, new StringBuilder()).toString();
    }

    default String mapKeyToString(OtpErlangObject obj) {
        return toString(obj) + " => ";
    }

    String emptyTupleString();

    String tupleLeftParen();

    String tupleRightParen();

    String emptyListString();

    String listLeftParen();

    String listRightParen();

    String mapLeft(OtpErlangObject obj);

    String mapRight();

    // True if given key in map should be hidden (Elixir __struct__)
    Boolean isHiddenField(OtpErlangObject key);

    String cons();
}

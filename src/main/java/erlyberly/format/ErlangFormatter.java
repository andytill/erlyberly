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

import java.util.ArrayList;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

public class ErlangFormatter implements TermFormatter {

    @Override
    public StringBuilder appendToString(OtpErlangObject obj, StringBuilder sb) {
        if(obj instanceof OtpErlangBinary) {
            sb.append("<<");
            Formatting.binaryToString((OtpErlangBinary) obj, ", ", sb);
            sb.append(">>");
        }
        else if(obj instanceof OtpErlangPid) {
            sb.append(pidToString((OtpErlangPid) obj));
        }
        else if(OtpUtil.isErlyberlyRecord(obj)) {
            OtpErlangTuple record = (OtpErlangTuple) obj;
            OtpErlangAtom recordName = (OtpErlangAtom) record.elementAt(1);
            OtpErlangList fields = (OtpErlangList) record.elementAt(2);
            sb.append("{").append(recordName).append(", ");
            for(int i=0; i < fields.arity(); i++) {
                if(i != 0) {
                    sb.append(", ");
                }
                appendToString(fields.elementAt(i), sb);
            }
            sb.append("}");
        }
        else if(OtpUtil.isErlyberlyRecordField(obj)) {
            OtpErlangObject fieldObj = ((OtpErlangTuple)obj).elementAt(2);
            appendToString(fieldObj, sb);
        }
        else if(obj instanceof OtpErlangTuple || obj instanceof OtpErlangList) {
            String brackets = bracketsForTerm(obj);
            OtpErlangObject[] elements = OtpUtil.elementsForTerm(obj);

            sb.append(brackets.charAt(0));

            for(int i=0; i < elements.length; i++) {
                if(i != 0) {
                    sb.append(", ");
                }
                appendToString(elements[i], sb);
            }

            if(obj instanceof OtpErlangList && !((OtpErlangList)obj).isProper()) {
                sb.append(cons());
                appendToString(((OtpErlangList)obj).getLastTail(), sb);
            }

            sb.append(brackets.charAt(1));
        }
        else if(obj instanceof OtpErlangString) {
            Formatting.appendString((OtpErlangString) obj, this, "\"", sb);
        }
        else {
            sb.append(obj.toString());
        }
        return sb;
    }

    private static String pidToString(OtpErlangPid pid) {
        return "<" + pid.node().toString() + "." + pid.id() + "." + pid.serial() + ">";
    }

    public String bracketsForTerm(OtpErlangObject obj) {
        assert obj != null;

        if(obj instanceof OtpErlangTuple)
            return "{}";
        else if(obj instanceof OtpErlangList)
            return "[]";
        else
            throw new RuntimeException("No brackets for type " + obj.getClass());
    }

    /**
     * Convert an MFA tuple to a string, where the MFA must have the type:
     *
     * {Module::atom(), Function::atom(), Args::[any()]}.
     */
    @Override
    public String modFuncArgsToString(OtpErlangTuple mfa) {
        StringBuilder sb = new StringBuilder();
        sb.append(atomToStringNoQuotes((OtpErlangAtom) mfa.elementAt(0)))
          .append(":")
          .append(atomToStringNoQuotes((OtpErlangAtom) mfa.elementAt(1)))
          .append("(");
        OtpErlangList args = (OtpErlangList) mfa.elementAt(2);
        ArrayList<String> stringArgs = new ArrayList<>();
        for (OtpErlangObject arg : args) {
            stringArgs.add(toString(arg));
        }
        sb.append(String.join(", ", stringArgs));
        sb.append(")");
        return sb.toString();
    }

    private String atomToStringNoQuotes(OtpErlangAtom atom) {
        return atom.atomValue();
    }

    @Override
    public String modFuncArityToString(OtpErlangTuple mfa) {
        StringBuilder sb = new StringBuilder();
        OtpErlangList argsList = OtpUtil.toErlangList(mfa.elementAt(2));
        sb.append(atomToStringNoQuotes((OtpErlangAtom) mfa.elementAt(0)))
          .append(":")
          .append(atomToStringNoQuotes((OtpErlangAtom) mfa.elementAt(1)))
          .append("/").append(argsList.arity());
        return sb.toString();
    }

    @Override
    public String exceptionToString(OtpErlangAtom errorClass, OtpErlangObject errorReason) {
        return errorClass + ":" +  toString(errorReason);
    }

    @Override
    public String emptyTupleString() {
        return "{ }";
    }

    @Override
    public String tupleLeftParen() {
        return "{";
    }

    @Override
    public String tupleRightParen() {
        return "}";
    }

    @Override
    public String emptyListString() {
        return "[ ]";
    }

    @Override
    public String listLeftParen() {
        return "[";
    }

    @Override
    public String listRightParen() {
        return "]";
    }

    @Override
    public String mapLeft(OtpErlangObject obj) {
        return "#{";
    }

    @Override
    public Boolean isHiddenField(OtpErlangObject key) {return false;}

    @Override
    public String mapRight() {
        return "}";
    }
    @Override
    public String cons() {
        return "|";
    }
}

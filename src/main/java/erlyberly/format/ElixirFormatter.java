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

import com.ericsson.otp.erlang.*;
import erlyberly.node.OtpUtil;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class ElixirFormatter implements TermFormatter {

    private static final String STRUCT_KEY = "__struct__";

    private static String stripElixirPrefix(final String str) {
        if (str.startsWith("'Elixir.")) {
            return ElixirFormatter.stripQutes(str).substring(7);
        }
        if (str.startsWith("Elixir.")) {
            return str.substring(7);
        }
        return str;
    }

    private static String stripQutes(final String str) {
        return str.substring(1, str.length() - 1);
    }

    @Override
    public StringBuilder appendToString(final OtpErlangObject obj, final StringBuilder sb) {
        if (obj instanceof OtpErlangBinary) {
            final OtpErlangBinary bin = (OtpErlangBinary) obj;
            if (Formatting.isDisplayableString(bin)) {
                sb.append("\"");
                sb.append(new String(bin.binaryValue(), StandardCharsets.UTF_8));
                sb.append("\"");
            } else {
                sb.append("<<");
                Formatting.binaryToString(bin, ", ", sb);
                sb.append(">>");
            }
        } else if (obj instanceof OtpErlangBitstr) {
            sb.append("<<");
            Formatting.bitstringToString((OtpErlangBitstr) obj, ", ", "%d::size(%d)", sb);
            sb.append(">>");
        } else if (obj instanceof OtpErlangPid) {
            sb.append(pidToString((OtpErlangPid) obj));
        } else if (OtpUtil.isErlyberlyRecord(obj)) {
            final OtpErlangTuple record = (OtpErlangTuple) obj;
            final OtpErlangAtom recordName = (OtpErlangAtom) record.elementAt(1);
            final OtpErlangList fields = (OtpErlangList) record.elementAt(2);
            sb.append("{").append(recordName).append(", ");
            for (int i = 0; i < fields.arity(); i++) {
                if (0 != i) {
                    sb.append(", ");
                }
                this.appendToString(fields.elementAt(i), sb);
            }
            sb.append("}");
        } else if (OtpUtil.isErlyberlyRecordField(obj)) {
            final OtpErlangObject fieldObj = ((OtpErlangTuple) obj).elementAt(2);
            this.appendToString(fieldObj, sb);
        } else if (obj instanceof OtpErlangTuple || obj instanceof OtpErlangList) {
            final String brackets = ElixirFormatter.bracketsForTerm(obj);
            final OtpErlangObject[] elements = OtpUtil.elementsForTerm(obj);

            sb.append(brackets.charAt(0));

            for (int i = 0; i < elements.length; i++) {
                if (0 != i) {
                    sb.append(", ");
                }
                this.appendToString(elements[i], sb);
            }

            if (obj instanceof OtpErlangList && !((OtpErlangList) obj).isProper()) {
                sb.append(this.cons());
                this.appendToString(((OtpErlangList) obj).getLastTail(), sb);
            }

            sb.append(brackets.charAt(1));
        } else if (obj instanceof OtpErlangString) {
            Formatting.appendString((OtpErlangString) obj, this, "'", sb);
        } else if (obj instanceof OtpErlangAtom) {
            String str = obj.toString();
            if (isaBoolean(str)) {
                final String innerStr = str.substring(1, str.length() - 1);
                if (innerStr.startsWith("Elixir.")) {
                    // Treat modules differently
                    str = innerStr.substring(7);
                } else {
                    // Convert Erlang style escaped atoms to Elixir style
                    str = ":\"" + innerStr + "\"";
                }

            } else {
                str = ":" + str;
            }
            sb.append(str);
        } else if (obj instanceof OtpErlangMap) {
            final OtpErlangMap map = (OtpErlangMap) obj;
            sb.append(this.mapLeft(obj));
            final Iterator<Map.Entry<OtpErlangObject, OtpErlangObject>> elemIt = map.entrySet().iterator();
            while (elemIt.hasNext()) {
                final Map.Entry<OtpErlangObject, OtpErlangObject> elem = elemIt.next();
                if (elem.getKey() instanceof OtpErlangAtom) {
                    if (this.isHiddenField(elem.getKey()).booleanValue()) {
                        continue;
                    }
                    sb.append(elem.getKey().toString());
                    sb.append(": ");
                } else {
                    this.appendToString(elem.getKey(), sb);
                    sb.append(" => ");
                }
                this.appendToString(elem.getValue(), sb);
                if (elemIt.hasNext()) {
                    sb.append(", ");
                }
            }
            sb.append("}");
        } else {
            sb.append(obj.toString());
        }
        return sb;
    }

    private static boolean isaBoolean(final String str) {
        return !str.isEmpty() && '\'' == str.charAt(0) && '\'' == str.charAt(str.length() - 1);
    }

    @Override
    public String mapKeyToString(final OtpErlangObject obj) {
        if (obj instanceof OtpErlangAtom) {
            return obj + ": ";
        } else {
            return this.appendToString(obj, new StringBuilder()) + " => ";
        }
    }

    private static String pidToString(final OtpErlangPid pid) {
        return pid.toString();
    }

    private static String bracketsForTerm(final OtpErlangObject obj) {
        assert null != obj;

        if (obj instanceof OtpErlangTuple) return "{}";
        else if (obj instanceof OtpErlangList) return "[]";
        else throw new RuntimeException("No brackets for type " + obj.getClass());
    }

    /**
     * Convert an MFA tuple to a string, where the MFA must have the type:
     * <p>
     * {Module::atom(), Function::atom(), Args::[any()]}.
     */
    @Override
    public String modFuncArgsToString(final OtpErlangTuple mfa) {
        final StringBuilder sb = new StringBuilder();
        sb.append(this.moduleNameToString((OtpErlangAtom) mfa.elementAt(0))).append(".").append(ElixirFormatter.funToStringNoQuotes((OtpErlangAtom) mfa.elementAt(1))).append("(");
        final OtpErlangList args = (OtpErlangList) mfa.elementAt(2);
        final List<String> stringArgs = new ArrayList<>();
        for (final OtpErlangObject arg : args) {
            stringArgs.add(this.toString(arg));
        }
        sb.append(String.join(", ", stringArgs));
        sb.append(")");
        return sb.toString();
    }

    private String moduleNameToString(final OtpErlangAtom mod) {
        final String str = mod.atomValue();
        return this.moduleNameToString(str);
    }

    @Override
    public String moduleNameToString(final String moduleName) {
        if (moduleName.startsWith("Elixir.")) return moduleName.substring(7);
        else return ":" + moduleName;
    }

    private static String funToStringNoQuotes(final OtpErlangAtom atom) {
        return atom.atomValue();
    }

    @Override
    public String modFuncArityToString(final OtpErlangTuple mfa) {
        final StringBuilder sb = new StringBuilder();
        final OtpErlangList argsList = OtpUtil.toErlangList(mfa.elementAt(2));
        sb.append(this.moduleNameToString((OtpErlangAtom) mfa.elementAt(0))).append(".").append(ElixirFormatter.funToStringNoQuotes((OtpErlangAtom) mfa.elementAt(1))).append("/").append(argsList.arity());
        return sb.toString();
    }

    @Override
    public String modFuncArityToString(final String mod, final String func, final int arity) {
        return this.moduleNameToString(mod) + "." + func + "/" + arity;
    }

    @Override
    public String exceptionToString(final OtpErlangAtom errorClass, final OtpErlangObject errorReason) {
        return errorClass + ":" + this.toString(errorReason);
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
    public String mapLeft(final OtpErlangObject obj) {
        final OtpErlangMap map = (OtpErlangMap) obj;
        final OtpErlangAtom structNameKey = new OtpErlangAtom(STRUCT_KEY);
        if (null != map.get(structNameKey)) {
            final String structName = ElixirFormatter.stripElixirPrefix(map.get(structNameKey).toString());
            return "%" + structName + "{";
        }
        return "%{";
    }

    @Override
    public Boolean isHiddenField(final OtpErlangObject key) {
        final OtpErlangAtom structNameKey = new OtpErlangAtom(STRUCT_KEY);
        return (key instanceof OtpErlangAtom) && key.equals(structNameKey);
    }

    @Override
    public String mapRight() {
        return "}";
    }

    @Override
    public String cons() {
        return "|";
    }
}

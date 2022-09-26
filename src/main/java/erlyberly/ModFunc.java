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
package erlyberly;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ModFunc implements Comparable<ModFunc> {

    private final String moduleName;

    private final String funcName;

    private final int arity;

    private final boolean exported;

    private final boolean synthetic;

    public ModFunc(String moduleName, String funcName, int arity, boolean exported, boolean synthetic) {
        this.moduleName = (moduleName != null) ? moduleName.replace("'", "") : null;
        this.funcName = (funcName != null) ? funcName.replace("'", "") : null;
        this.arity = arity;
        this.exported = exported;
        this.synthetic = synthetic;
    }

    public String getModuleName() {
        return moduleName;
    }

    public String getFuncName() {
        return funcName;
    }

    public int getArity() {
        return arity;
    }

    public boolean isExported() {
        return exported;
    }

    public boolean isSynthetic() {
        return synthetic;
    }

    @Override
    public String toString() {
        if (funcName == null) {
            return ErlyBerly.getTermFormatter().moduleNameToString(moduleName);
        }
        return funcName + "/" + arity;
    }

    public String toFullString() {
        if (funcName == null) {
            return ErlyBerly.getTermFormatter().moduleNameToString(moduleName);
        }
        return ErlyBerly.getTermFormatter().modFuncArityToString(moduleName, funcName, arity);
    }

    public static ModFunc toFunc(OtpErlangAtom moduleName, OtpErlangObject e, boolean exported) {
        OtpErlangAtom funcNameAtom = (OtpErlangAtom) ((OtpErlangTuple) e).elementAt(0);
        OtpErlangLong arity = (OtpErlangLong) ((OtpErlangTuple) e).elementAt(1);

        String funcName = funcNameAtom.atomValue();

        return new ModFunc(moduleName.atomValue(), funcName, (int) arity.longValue(), exported, funcName.startsWith("-"));
    }

    public static ModFunc toModule(OtpErlangAtom moduleName) {
        return new ModFunc(moduleName.atomValue(), null, 0, false, false);
    }

    @Override
    public int compareTo(ModFunc o) {
        if (funcName == null) {
            return moduleName.compareTo(o.moduleName);
        }

        int comp = funcName.compareTo(o.funcName);
        if (comp == 0) {
            comp = Integer.compare(arity, o.arity);
        }
        return comp;
    }

    public boolean isModule() {
        return (funcName == null);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + arity;
        result = prime * result + ((funcName == null) ? 0 : funcName.hashCode());
        result = prime * result + ((moduleName == null) ? 0 : moduleName.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        ModFunc other = (ModFunc) obj;
        if (arity != other.arity) return false;
        if (funcName == null) {
            if (other.funcName != null) return false;
        } else if (!funcName.equals(other.funcName)) return false;
        if (moduleName == null) {
            return other.moduleName == null;
        } else return moduleName.equals(other.moduleName);
    }

    public boolean isModuleInfo() {
        return "module_info".equals(funcName) && (arity == 0 || arity == 1);
    }


}

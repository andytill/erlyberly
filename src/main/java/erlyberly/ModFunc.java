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

    public ModFunc(final String moduleName, final String funcName, final int arity, final boolean exported, final boolean synthetic) {
        super();
        this.moduleName = (null != moduleName) ? moduleName.replace("'", "") : null;
        this.funcName = (null != funcName) ? funcName.replace("'", "") : null;
        this.arity = arity;
        this.exported = exported;
        this.synthetic = synthetic;
    }

    public String getModuleName() {
        return this.moduleName;
    }

    public String getFuncName() {
        return this.funcName;
    }

    public int getArity() {
        return this.arity;
    }

    public boolean isExported() {
        return this.exported;
    }

    public boolean isSynthetic() {
        return this.synthetic;
    }

    @Override
    public String toString() {
        if (null == this.funcName) {
            return ErlyBerly.getTermFormatter().moduleNameToString(this.moduleName);
        }
        return this.funcName + "/" + this.arity;
    }

    public String toFullString() {
        if (null == this.funcName) {
            return ErlyBerly.getTermFormatter().moduleNameToString(this.moduleName);
        }
        return ErlyBerly.getTermFormatter().modFuncArityToString(this.moduleName, this.funcName, this.arity);
    }

    public static ModFunc toFunc(final OtpErlangAtom moduleName, final OtpErlangObject e, final boolean exported) {
        final OtpErlangAtom funcNameAtom = (OtpErlangAtom) ((OtpErlangTuple) e).elementAt(0);
        final OtpErlangLong arity = (OtpErlangLong) ((OtpErlangTuple) e).elementAt(1);

        final String funcName = funcNameAtom.atomValue();

        return new ModFunc(moduleName.atomValue(), funcName, (int) arity.longValue(), exported, !funcName.isEmpty() && '-' == funcName.charAt(0));
    }

    public static ModFunc toModule(final OtpErlangAtom moduleName) {
        return new ModFunc(moduleName.atomValue(), null, 0, false, false);
    }

    @Override
    public int compareTo(final ModFunc o) {
        if (null == this.funcName) {
            return this.moduleName.compareTo(o.moduleName);
        }

        int comp = this.funcName.compareTo(o.funcName);
        if (0 == comp) {
            comp = Integer.compare(this.arity, o.arity);
        }
        return comp;
    }

    public boolean isModule() {
        return (null == this.funcName);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + this.arity;
        result = prime * result + ((null == this.funcName) ? 0 : this.funcName.hashCode());
        result = prime * result + ((null == this.moduleName) ? 0 : this.moduleName.hashCode());
        return result;
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) return true;
        if (null == obj) return false;
        if (this.getClass() != obj.getClass()) return false;
        final ModFunc other = (ModFunc) obj;
        if (this.arity != other.arity) return false;
        if (null == this.funcName) {
            if (null != other.funcName) return false;
        } else if (!this.funcName.equals(other.funcName)) return false;
        if (null == this.moduleName) {
            return null == other.moduleName;
        } else return this.moduleName.equals(other.moduleName);
    }

    public boolean isModuleInfo() {
        return !"module_info".equals(this.funcName) || (0 != this.arity && 1 != this.arity);
    }


}

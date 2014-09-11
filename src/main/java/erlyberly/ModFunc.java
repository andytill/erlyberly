package erlyberly;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ModFunc implements Comparable<ModFunc> {

	private final String moduleName;
	
	private final String funcName;
	
	private final int arity;
	
	private final boolean exported;
	
	private final boolean synthetic;

	public ModFunc(String moduleName, String funcName, int arity, boolean exported, boolean synthetic) {
		this.moduleName = moduleName;
		this.funcName = funcName;
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
		if(funcName == null) {
			return moduleName;
		}
		return funcName + "/" + arity;
	}
	
	public String toFullString() {
		if(funcName == null) {
			return moduleName;
		}
		return moduleName + ":" + funcName + "/" + arity;
	}

	public static ModFunc toFunc(OtpErlangAtom moduleName, OtpErlangObject e, boolean exported) throws OtpErlangRangeException {
		OtpErlangAtom funcNameAtom = (OtpErlangAtom) ((OtpErlangTuple) e).elementAt(0);
		OtpErlangLong arity = (OtpErlangLong) ((OtpErlangTuple) e).elementAt(1);
		
		String funcName = funcNameAtom.atomValue();
		
		return new ModFunc(
			moduleName.atomValue(), 
			funcName, 
			arity.intValue(), 
			exported,
			funcName.startsWith("-")
		);
	}

	public static ModFunc toModule(OtpErlangAtom moduleName) {
		return new ModFunc(
			moduleName.atomValue(), 
			null, 
			0, 
			false,
			false
		);
	}

	@Override
	public int compareTo(ModFunc o) {
		if(funcName == null) {
			return moduleName.compareTo(o.moduleName);
		}
		
		int comp = funcName.compareTo(o.funcName);
		if(comp == 0) {
			comp =  Integer.compare(arity, o.arity);
		}
		return comp;
	}
}

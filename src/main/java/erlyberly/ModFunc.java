package erlyberly;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ModFunc {
	
	private final String name;
	
	private final int arity;
	
	private final boolean exported;

	public ModFunc(String name, int arity, boolean exported) {
		this.name = name;
		this.arity = arity;
		this.exported = exported;
	}

	public String getName() {
		return name;
	}

	public int getArity() {
		return arity;
	}

	public boolean isExported() {
		return exported;
	}

	@Override
	public String toString() {
		return name + "/" + arity;
	}
	
	public static ModFunc toModFunc(OtpErlangObject e, boolean exported) throws OtpErlangRangeException {
		OtpErlangAtom funcName = (OtpErlangAtom) ((OtpErlangTuple) e).elementAt(0);
		OtpErlangLong arity = (OtpErlangLong) ((OtpErlangTuple) e).elementAt(1);
		
		return new ModFunc(funcName.atomValue(), arity.intValue(), exported);
	}
}

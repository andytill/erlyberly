package erlyberly;

import java.util.ArrayList;
import java.util.HashMap;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

public class TraceLog {
	
	private static final OtpErlangAtom RESULT_ATOM = new OtpErlangAtom("result");
	private static final OtpErlangAtom ATOM_PID = new OtpErlangAtom("pid");
	private static final OtpErlangAtom ATOM_REG_NAME = new OtpErlangAtom("reg_name");
	private static final OtpErlangAtom ATOM_UNDEFINED = new OtpErlangAtom("undefined");

	private HashMap<Object, Object> map;

	public TraceLog(HashMap<Object, Object> map) {
		this.map = map;
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public String toString() {
		return tracePropsToString();
	}
	

	
	private String tracePropsToString() {
		String trace = "";
		
		OtpErlangAtom regName = (OtpErlangAtom) map.get(ATOM_REG_NAME);
		
		if(!ATOM_UNDEFINED.equals(regName)) {
			trace += regName.atomValue();
		}
		else {
			OtpErlangString pidString = (OtpErlangString) map.get(ATOM_PID);
			trace += pidString.stringValue();
		}
		trace += " ";
		trace += fnToFunctionString((OtpErlangTuple)map.get(new OtpErlangAtom("fn")));
		trace += " => ";
		trace += OtpUtil.otpObjectToString((OtpErlangObject) map.get(RESULT_ATOM));
		
		return trace;
	}
	
	private String fnToFunctionString(OtpErlangTuple tuple) {
		OtpErlangAtom mod = (OtpErlangAtom) tuple.elementAt(0);
		OtpErlangAtom func = (OtpErlangAtom) tuple.elementAt(1);
		OtpErlangList args = (OtpErlangList) tuple.elementAt(2);
		ArrayList<String> sargs = new ArrayList<String>();
		for (OtpErlangObject otpErlangObject : args) {
			sargs.add(otpErlangObject.toString());
		}
		
		String join = String.join(", ", sargs);
		
		String fn = mod.atomValue() + ":" + func.atomValue() + "(" + join + ")";
		
		return fn;
	}

	public OtpErlangObject getArgs() {
		OtpErlangTuple tuple = (OtpErlangTuple) map.get(new OtpErlangAtom("fn"));
		OtpErlangList args = (OtpErlangList) tuple.elementAt(2);
		return args;
	}

	public OtpErlangObject getResult() {
		return (OtpErlangObject) map.get(RESULT_ATOM);
	}
}

package erlyberly;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicLong;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

public class TraceLog implements Comparable<TraceLog> {

	private static final OtpErlangAtom EXCEPTION_FROM_ATOM = new OtpErlangAtom("exception_from");
	private static final OtpErlangAtom RESULT_ATOM = new OtpErlangAtom("result");
	private static final OtpErlangAtom ATOM_PID = new OtpErlangAtom("pid");
	private static final OtpErlangAtom ATOM_REG_NAME = new OtpErlangAtom("reg_name");
	private static final OtpErlangAtom ATOM_UNDEFINED = new OtpErlangAtom("undefined");
	
	private static final AtomicLong instanceCounter = new AtomicLong();

	private final HashMap<Object, Object> map;
	
	private final long instanceNum;
	
	private String toString;

	public TraceLog(HashMap<Object, Object> map) {
		this.map = map;
		instanceNum = instanceCounter.incrementAndGet();
	}
	
	@Override
	public String toString() {
		if(toString == null) {
			toString = tracePropsToString();
		}
		return toString;
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
		
		if(map.containsKey(RESULT_ATOM)) {
			trace += OtpUtil.otpObjectToString((OtpErlangObject) map.get(RESULT_ATOM));
		}
		else if(map.containsKey(EXCEPTION_FROM_ATOM)) {
			OtpErlangTuple exception = (OtpErlangTuple) map.get(EXCEPTION_FROM_ATOM);
			
			String clazz = OtpUtil.otpObjectToString(exception.elementAt(0));
			String reason = OtpUtil.otpObjectToString(exception.elementAt(1));
			trace += clazz + ":" +  reason;
		}
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
		Object object = map.get(RESULT_ATOM);
		if(object == null) {
			OtpErlangTuple exception = (OtpErlangTuple) map.get(EXCEPTION_FROM_ATOM);
			
			return exception.elementAt(1);
		}
		return (OtpErlangObject) object;
	}

	@Override
	public int compareTo(TraceLog o) {
		return Long.compare(instanceNum, o.instanceNum);
	}
}

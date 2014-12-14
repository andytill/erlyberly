package erlyberly;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicLong;

import javafx.application.Platform;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

public class TraceLog implements Comparable<TraceLog> {

	public static final OtpErlangAtom EXCEPTION_FROM_ATOM = new OtpErlangAtom("exception_from");
	public static final OtpErlangAtom RESULT_ATOM = new OtpErlangAtom("result");
	public static final OtpErlangAtom ATOM_PID = new OtpErlangAtom("pid");
	public static final OtpErlangAtom ATOM_REG_NAME = new OtpErlangAtom("reg_name");
	public static final OtpErlangAtom ATOM_UNDEFINED = new OtpErlangAtom("undefined");
	
	private static final AtomicLong instanceCounter = new AtomicLong();

	private final HashMap<Object, Object> map;
	
	private final long instanceNum;
	
	private final SimpleStringProperty summary = new SimpleStringProperty("");
	
	private final SimpleBooleanProperty complete = new SimpleBooleanProperty(false);
	

	public TraceLog(HashMap<Object, Object> map) {
		this.map = map;
		instanceNum = instanceCounter.incrementAndGet();
	}
	
	public SimpleStringProperty summaryProperty() {
		if(summary.get().isEmpty()) {
			summary.set(toString());
		}
		return summary;
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
		
		if(map.containsKey(RESULT_ATOM)) {
			trace += OtpUtil.otpObjectToString((OtpErlangObject) map.get(RESULT_ATOM));
		}
		else if(isExceptionThrower()) {
			OtpErlangTuple exception = (OtpErlangTuple) map.get(EXCEPTION_FROM_ATOM);
			
			String clazz = OtpUtil.otpObjectToString(exception.elementAt(0));
			String reason = OtpUtil.otpObjectToString(exception.elementAt(1));
			trace += clazz + ":" +  reason;
		}
		return trace;
	}

	public boolean isExceptionThrower() {
		return map.containsKey(EXCEPTION_FROM_ATOM);
	}
	
	private String fnToFunctionString(OtpErlangTuple tuple) {
		OtpErlangAtom mod = (OtpErlangAtom) tuple.elementAt(0);
		OtpErlangAtom func = (OtpErlangAtom) tuple.elementAt(1);
		
		// arguments of one or more integers that all come within the ASCII 
		// range can get miscast as OtpErlangString instances
		OtpErlangList args = OtpUtil.toOtpList(tuple.elementAt(2));
		
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
		OtpErlangList args = OtpUtil.toOtpList(tuple.elementAt(2));
		return args;
	}
	
	public String getPidString() {
		OtpErlangString s = (OtpErlangString)map.get(ATOM_PID);
		
		return s.stringValue();
	}

	public OtpErlangObject getResult() {
		Object object = map.get(RESULT_ATOM);
		if(object == null) {
			OtpErlangTuple exception = (OtpErlangTuple) map.get(EXCEPTION_FROM_ATOM);
			
			if(exception != null) {
				return exception.elementAt(1);
			}
		}
		return (OtpErlangObject) object;
	}

	@Override
	public int compareTo(TraceLog o) {
		return Long.compare(instanceNum, o.instanceNum);
	}

	public void complete(HashMap<Object, Object> resultMap) {
		Object e = resultMap.get(EXCEPTION_FROM_ATOM);
		Object r = resultMap.get(RESULT_ATOM);
		
		if(e != null)
			map.put(EXCEPTION_FROM_ATOM, e);
		if(r != null)
			map.put(RESULT_ATOM, r);
		
		Platform.runLater(() -> { summary.set(toString()); complete.set(true); });
	}
	
	public ReadOnlyBooleanProperty isCompleteProperty() {
		return complete;
	}
}

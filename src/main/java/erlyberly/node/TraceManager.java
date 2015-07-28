package erlyberly.node;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Stack;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.TraceLog;

public class TraceManager {

	private static final OtpErlangAtom RETURN_FROM_ATOM = new OtpErlangAtom("return_from");
	
	private static final OtpErlangAtom EXCEPTION_FROM_ATOM = new OtpErlangAtom("exception_from");

	private static final OtpErlangAtom CALL_ATOM = new OtpErlangAtom("call");
	
	private final HashMap<String, Stack<TraceLog>> unfinishedCalls = new HashMap<String, Stack<TraceLog>>();

	public ArrayList<TraceLog> collateTraces(OtpErlangList traceLogs) {
		ArrayList<TraceLog> traceList = new ArrayList<TraceLog>();
		
		for (OtpErlangObject otpErlangObject : traceLogs) {
			OtpErlangTuple tup = (OtpErlangTuple) otpErlangObject;
			OtpErlangAtom traceType = (OtpErlangAtom) tup.elementAt(0);
			
			if(CALL_ATOM.equals(traceType)) {
				TraceLog trace = proplistToTraceLog(tup);

				Stack<TraceLog> stack = unfinishedCalls.get(trace.getPidString());
				
				if(stack == null)
					stack = new Stack<TraceLog>();
				
				stack.add(trace);
				
				unfinishedCalls.put(trace.getPidString(), stack);
				
				traceList.add(trace);
			}
			else if(RETURN_FROM_ATOM.equals(traceType) || EXCEPTION_FROM_ATOM.equals(traceType)) {
				HashMap<Object, Object> map = propsFromTrace(tup);
				
				Object object = map.get(TraceLog.ATOM_PID);
				
				if(object != null) {
					OtpErlangString pidString = (OtpErlangString) object;
					
					Stack<TraceLog> stack = unfinishedCalls.get(pidString.stringValue());
					if(stack == null)
						continue;
					
					TraceLog traceLog = stack.pop();
					traceLog.complete(map);
					
					if(stack.isEmpty())
						unfinishedCalls.remove(pidString.stringValue());
				}
				
			}
				
		}
		
		return traceList;
	}

	private TraceLog proplistToTraceLog(OtpErlangTuple tup) {
		HashMap<Object, Object> map = propsFromTrace(tup);
		
		TraceLog trace = new TraceLog(map);
		
		return trace;
	}

	private HashMap<Object, Object> propsFromTrace(OtpErlangTuple tup) {
		return OtpUtil.propsToMap((OtpErlangList) tup.elementAt(1));
	}
}

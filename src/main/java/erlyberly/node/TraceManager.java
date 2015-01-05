package erlyberly.node;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Queue;

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
	
	private final HashMap<String, Queue<TraceLog>> unfinishedCalls = new HashMap<String, Queue<TraceLog>>();

	public ArrayList<TraceLog> collateTraces(OtpErlangList traceLogs) {
		ArrayList<TraceLog> traceList = new ArrayList<TraceLog>();
		
		for (OtpErlangObject otpErlangObject : traceLogs) {
			OtpErlangTuple tup = (OtpErlangTuple) otpErlangObject;
			OtpErlangAtom traceType = (OtpErlangAtom) tup.elementAt(0);
			
			if(CALL_ATOM.equals(traceType)) {
				TraceLog trace = proplistToTraceLog(tup);

				Queue<TraceLog> queue = unfinishedCalls.get(trace.getPidString());
				
				if(queue == null)
					queue = new LinkedList<TraceLog>();
				
				queue.add(trace);
				
				unfinishedCalls.put(trace.getPidString(), queue);
				
				traceList.add(trace);
			}
			else if(RETURN_FROM_ATOM.equals(traceType) || EXCEPTION_FROM_ATOM.equals(traceType)) {
				HashMap<Object, Object> map = propsFromTrace(tup);
				
				Object object = map.get(TraceLog.ATOM_PID);
				
				if(object != null) {
					OtpErlangString pidString = (OtpErlangString) object;
					
					Queue<TraceLog> queue = unfinishedCalls.get(pidString.stringValue());
					TraceLog traceLog = queue.poll();
					traceLog.complete(map);
					
					if(queue.isEmpty())
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

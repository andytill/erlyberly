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
import javafx.application.Platform;
import javafx.scene.control.TreeItem;
import ui.TreeItemSF;

public class TraceManager {

	private static final OtpErlangAtom RETURN_FROM_ATOM = new OtpErlangAtom("return_from");
	
	private static final OtpErlangAtom EXCEPTION_FROM_ATOM = new OtpErlangAtom("exception_from");

	private static final OtpErlangAtom CALL_ATOM = new OtpErlangAtom("call");
	
	private final HashMap<String, Stack<TreeItemSF<TraceLog>>> unfinishedCalls = new HashMap<>();

	
	public ArrayList<TreeItem<TraceLog>> collateTraces(OtpErlangList traceLogs, ArrayList<TreeItem<TraceLog>> resultList) {
	    assert Platform.isFxApplicationThread();
		
		for (OtpErlangObject obj : traceLogs) {
		    decodeTraceLog(obj, resultList);
		}
		return resultList;
	}
	
	private void decodeTraceLog(OtpErlangObject otpErlangObject, ArrayList<TreeItem<TraceLog>> traceList) {
	    OtpErlangTuple tup = (OtpErlangTuple) otpErlangObject;
        OtpErlangAtom traceType = (OtpErlangAtom) tup.elementAt(0);
        
        if(CALL_ATOM.equals(traceType)) {
            TraceLog trace = proplistToTraceLog(tup);
            TreeItemSF<TraceLog> traceItem = new TreeItemSF<>(trace);
            traceItem.setExpanded(true);
            Stack<TreeItemSF<TraceLog>> stack = unfinishedCalls.get(trace.getPidString());
            
            boolean is_child = false;
            if(stack == null) {
                stack = new Stack<>();
            }
            else {
                TreeItemSF<TraceLog> peek = stack.peek();
                if(peek != null) {
                    is_child = true;
                    peek.getInputItems().add(traceItem);
                }
            }
            stack.add(traceItem);
            
            unfinishedCalls.put(trace.getPidString(), stack);
            
            if(!is_child) {
                traceList.add(traceItem);
            }
        }
        else if(RETURN_FROM_ATOM.equals(traceType) || EXCEPTION_FROM_ATOM.equals(traceType)) {
            HashMap<Object, Object> map = propsFromTrace(tup);
            
            Object object = map.get(TraceLog.ATOM_PID);
            
            if(object != null) {
                OtpErlangString pidString = (OtpErlangString) object;
                
                Stack<TreeItemSF<TraceLog>> stack = unfinishedCalls.get(pidString.stringValue());
                if(stack == null)
                    return;
                
                TreeItem<TraceLog> traceLog = stack.pop();
                traceLog.getValue().complete(map);
                
                if(stack.isEmpty())
                    unfinishedCalls.remove(pidString.stringValue());
            }
            
        }
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

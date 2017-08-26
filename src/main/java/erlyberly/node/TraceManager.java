/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly.node;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Stack;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.ErlyBerly;
import erlyberly.TraceLog;

public class TraceManager {

    private static final OtpErlangAtom RETURN_FROM_ATOM = new OtpErlangAtom("return_from");

    private static final OtpErlangAtom EXCEPTION_FROM_ATOM = new OtpErlangAtom("exception_from");

    private static final OtpErlangAtom CALL_ATOM = new OtpErlangAtom("call");

    private final HashMap<OtpErlangPid, Stack<TraceLog>> unfinishedCalls = new HashMap<OtpErlangPid, Stack<TraceLog>>();

    /**
     * Called when a trace log is received.
     * <br/>
     * Should only accessed from the FX thread.
     */
    private RpcCallback<TraceLog> traceLogCallback;

    public List<TraceLog> collateTraces(OtpErlangList traceLogs) {
        final ArrayList<TraceLog> traceList = new ArrayList<TraceLog>();

        for (OtpErlangObject obj : traceLogs) {
            decodeTraceLog(obj, traceList);
        }

        return traceList;
    }

    public List<TraceLog> collateTraceSingle(OtpErlangTuple traceLog) {
        final ArrayList<TraceLog> traceList = new ArrayList<TraceLog>();
        decodeTraceLog(traceLog, traceList);
        return traceList;
    }

    private void decodeTraceLog(OtpErlangObject otpErlangObject, ArrayList<TraceLog> traceList) {
        OtpErlangTuple tup = (OtpErlangTuple) otpErlangObject;
        OtpErlangAtom traceType = (OtpErlangAtom) tup.elementAt(2);

        if(CALL_ATOM.equals(traceType)) {
            TraceLog trace = proplistToTraceLog(tup);

            Stack<TraceLog> stack = unfinishedCalls.get(trace.getPid());

            if(stack == null)
                stack = new Stack<TraceLog>();

            stack.add(trace);

            unfinishedCalls.put(trace.getPid(), stack);

            if(traceLogCallback != null)
                traceLogCallback.callback(trace);
        }
        else if(RETURN_FROM_ATOM.equals(traceType) || EXCEPTION_FROM_ATOM.equals(traceType)) {
            assert tup.elementAt(1) instanceof OtpErlangPid;
            OtpErlangPid tracedPid = (OtpErlangPid) tup.elementAt(1);
            Stack<TraceLog> stack = unfinishedCalls.get(tracedPid);
            if(stack == null)
                return;
            TraceLog traceLog = stack.pop();
            traceLog.complete(tup, ErlyBerly.getTermFormatter());
            if(stack.isEmpty())
                unfinishedCalls.remove(tracedPid);
        }
        else {
            assert false : traceType;
        }
    }

    private TraceLog proplistToTraceLog(OtpErlangTuple tup) {
        TraceLog trace = new TraceLog(tup, ErlyBerly.getTermFormatter());

        return trace;
    }

    public RpcCallback<TraceLog> getTraceLogCallback() {
        return traceLogCallback;
    }

    public void setTraceLogCallback(RpcCallback<TraceLog> traceLogCallback) {
        this.traceLogCallback = traceLogCallback;
    }
}

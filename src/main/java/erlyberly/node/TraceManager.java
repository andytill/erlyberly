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
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.ErlyBerly;
import erlyberly.TraceLog;
import javafx.application.Platform;

public class TraceManager {

    private final HashMap<OtpErlangPid, Stack<TraceLog>> unfinishedCalls = new HashMap<OtpErlangPid, Stack<TraceLog>>();

    /**
     * A map from processes to their registered name. It is updated when
     * erlang:register/2 and erlang:unregister/1 calls are made, which are
     * traced when dbg is started.
     *
     * When a process goes down, it is not unregistered which means we may
     * end up with a lot of keys which are dead processes! FIXME
     */
    private final HashMap<OtpErlangPid, OtpErlangAtom> registeredProcesses = new HashMap<>();

    /**
     * A wrapper for trace tuples to give an API, rather than pull values
     * from the tuple.
     */
    private final TraceTuple traceTuple = new TraceTuple();

    /**
     * Called when a trace log is received.
     * <br/>
     * Should only accessed from the FX thread.
     */
    private RpcCallback<TraceLog> traceLogCallback;

    private RpcCallback<OtpErlangTuple> moduleLoadedCallback;

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
        traceTuple.tuple = (OtpErlangTuple) otpErlangObject;
        OtpErlangTuple tup = (OtpErlangTuple) otpErlangObject;
        OtpErlangAtom traceType = traceTuple.getTraceType();

        if(Atoms.CALL_ATOM.equals(traceType)) {
            OtpErlangAtom moduleLoad = traceTuple.isModuleLoad();
            if(moduleLoad != null) {
                // if a module is reloaded then we need to reload the functions for it
                // since some may be added or deleted. We also need to reapply traces
                // since a reload removes all traces on that module
                System.out.println("code reload");
                ErlyBerly.runIO(() -> {
                    try {
                        OtpErlangTuple moduleFunctions = ErlyBerly.nodeAPI().moduleFunctions(moduleLoad);
                        Platform.runLater(() -> { moduleLoadedCallback.callback(moduleFunctions); });
                    }
                    catch (Exception e) {
                        e.printStackTrace();
                    }
                });
                // don't show the module load to the user as a trace in the table
                return;
            }
            else if(traceTuple.isProcessRegistration()) {
                return;
            }
            TraceLog trace = proplistToTraceLog(tup);

            Stack<TraceLog> stack = unfinishedCalls.get(trace.getPid());

            if(stack == null)
                stack = new Stack<TraceLog>();

            stack.add(trace);

            unfinishedCalls.put(trace.getPid(), stack);

            if(traceLogCallback != null)
                traceLogCallback.callback(trace);
        }
        else if(Atoms.RETURN_FROM_ATOM.equals(traceType) || Atoms.EXCEPTION_FROM_ATOM.equals(traceType)) {
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

    class TraceTuple {
        OtpErlangTuple tuple;

        OtpErlangAtom getTraceType() {
            return (OtpErlangAtom) tuple.elementAt(2);
        }

        private OtpErlangAtom isModuleLoad() {
            if(!Atoms.CALL_ATOM.equals(getTraceType()))
                return null;
            OtpErlangTuple mfa = getMFA();
            OtpErlangObject mod = mfa.elementAt(0);
            OtpErlangObject function = mfa.elementAt(1);
            OtpErlangList args = (OtpErlangList) mfa.elementAt(2);
            if(Atoms.CODE_SERVER_ATOM.equals(mod) && Atoms.TRY_LOAD_MODULE_ATOM.equals(function)) {
                return (OtpErlangAtom) args.elementAt(1);
            }
            return null;
        }

        private boolean isProcessRegistration() {
            if(!Atoms.CALL_ATOM.equals(getTraceType()))
                return false;
            OtpErlangTuple mfa = getMFA();
            OtpErlangObject mod = mfa.elementAt(0);
            OtpErlangObject function = mfa.elementAt(1);
            OtpErlangList args = (OtpErlangList) mfa.elementAt(2);
            if(!Atoms.ERLANG_ATOM.equals(mod))
                return false;
            if(Atoms.REGISTER_ATOM.equals(function)) {
                OtpErlangAtom regName = (OtpErlangAtom) args.elementAt(0);
                OtpErlangPid regPid = (OtpErlangPid) args.elementAt(1);
                registeredProcesses.put(regPid, regName);
                return true;
            }
            else if(Atoms.UNREGISTER_ATOM.equals(function)) {
                OtpErlangAtom regName = (OtpErlangAtom) args.elementAt(0);
                OtpErlangPid regPid = null;
                for (Entry<OtpErlangPid, OtpErlangAtom> e : registeredProcesses.entrySet()) {
                    if(regName.equals(e.getValue())) {
                        regPid = e.getKey();
                        continue;
                    }
                }
                // do not try to remove the entry from within the loop because that may thrown
                // a ConcurrentModificationException
                if(regPid != null) {
                    registeredProcesses.remove(regPid);
                }
                return true;
            }
            return false;
        }

        private OtpErlangTuple getMFA() {
            return (OtpErlangTuple) tuple.elementAt(3);
        }
    }

    private TraceLog proplistToTraceLog(OtpErlangTuple tup) {
        TraceLog trace = new TraceLog(tup, registeredProcesses, ErlyBerly.getTermFormatter());

        return trace;
    }

    public RpcCallback<TraceLog> getTraceLogCallback() {
        return traceLogCallback;
    }

    public void setTraceLogCallback(RpcCallback<TraceLog> traceLogCallback) {
        this.traceLogCallback = traceLogCallback;
    }

    public void setModuleLoadedCallback(RpcCallback<OtpErlangTuple> aModuleLoadedCallback) {
        moduleLoadedCallback = aModuleLoadedCallback;

    }

    public void setRegisteredProcesses(Map<OtpErlangObject, OtpErlangObject> regProcs) {
        registeredProcesses.clear();
        for (Entry<OtpErlangObject, OtpErlangObject> e : regProcs.entrySet()) {
            registeredProcesses.put((OtpErlangPid)e.getKey(), (OtpErlangAtom)e.getValue());
        }
    }
}

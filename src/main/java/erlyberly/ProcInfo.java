package erlyberly;

import java.util.Map;
import java.util.Objects;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangString;

import javafx.beans.property.LongProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

/**
 * Domain object for an erlang process.
 */
public class ProcInfo implements Comparable<ProcInfo> {

    private static final OtpErlangAtom TOTAL_HEAP_SIZE_ATOM = new OtpErlangAtom("total_heap_size");

    private static final OtpErlangAtom STACK_SIZE_ATOM = new OtpErlangAtom("stack_size");

    private static final OtpErlangAtom HEAP_SIZE_ATOM = new OtpErlangAtom("heap_size");

    private static final OtpErlangAtom MSG_QUEUE_LEN_ATOM = new OtpErlangAtom("message_queue_len");

    private static final OtpErlangList EMPTY_LIST = new OtpErlangList();

    private static final OtpErlangAtom REGISTERED_NAME_ATOM = new OtpErlangAtom("registered_name");

    private static final OtpErlangAtom PID_ATOM = new OtpErlangAtom("pid");

    private static final OtpErlangAtom REDUCTIONS_ATOM = new OtpErlangAtom("reductions");

    private StringProperty pid;

    private StringProperty processName;

    private LongProperty reductions;

    private LongProperty msgQueueLen;

    private LongProperty heapSize;

    private LongProperty stackSize;

    private LongProperty totalHeapSize;

    public void setTotalHeapSize(long value) {
        totalHeapSizeProperty().set(value);
    }

    public long getTotalHeapSize() {
        return totalHeapSizeProperty().get();
    }

    public LongProperty totalHeapSizeProperty() {
        if (totalHeapSize == null)
            totalHeapSize = new SimpleLongProperty(this, "totalHeapSize");
        return totalHeapSize;
    }

    public void setStackSize(long value) {
        stackSizeProperty().set(value);
    }

    public long getStackSize() {
        return stackSizeProperty().get();
    }

    public LongProperty stackSizeProperty() {
        if (stackSize == null)
            stackSize = new SimpleLongProperty(this, "stackSize");
        return stackSize;
    }

    public void setHeapSize(long value) {
        heapSizeProperty().set(value);
    }

    public long getHeapSize() {
        return heapSizeProperty().get();
    }

    public LongProperty heapSizeProperty() {
        if (heapSize == null)
            heapSize = new SimpleLongProperty(this, "msgQueueLen");
        return heapSize;
    }

    public void setMsgQueueLen(long value) {
        msgQueueLenProperty().set(value);
    }

    public long getMsgQueueLen() {
        return msgQueueLenProperty().get();
    }

    public LongProperty msgQueueLenProperty() {
        if (msgQueueLen == null)
            msgQueueLen = new SimpleLongProperty(this, "msgQueueLen");
        return msgQueueLen;
    }

    public void setPid(String value) {
        pidProperty().set(value);
    }

    public String getPid() {
        return pidProperty().get();
    }

    public StringProperty pidProperty() {
        if (pid == null)
            pid = new SimpleStringProperty(this, "pid");
        return pid;
    }

    public void setProcessName(String value) {
        processNameProperty().set(value);
    }

    public String getProcessName() {
        return processNameProperty().get();
    }

    public StringProperty processNameProperty() {
        if (processName == null)
            processName = new SimpleStringProperty(this, "processName");
        return processName;
    }

    public void setReductions(long value) {
        reductionsProperty().set(value);
    }

    public long getReductions() {
        return reductionsProperty().get();
    }

    public LongProperty reductionsProperty() {
        if (reductions == null)
            reductions = new SimpleLongProperty(this, "reductions");
        return reductions;
    }

    public static ProcInfo toProcessInfo(Map<Object, Object> propList) {
        Object processName = propList.get(REGISTERED_NAME_ATOM);
        Object pid = ((OtpErlangString) propList.get(PID_ATOM)).stringValue();

        if(EMPTY_LIST.equals(processName)) {
            processName = "";
        }

        ProcInfo processInfo;

        processInfo = new ProcInfo();
        processInfo.setProcessName(Objects.toString(processName, ""));
        processInfo.setPid(Objects.toString(pid, ""));
        processInfo.setReductions(toLong(propList.get(REDUCTIONS_ATOM)));
        processInfo.setMsgQueueLen(toLong(propList.get(MSG_QUEUE_LEN_ATOM)));
        processInfo.setHeapSize(toLong(propList.get(HEAP_SIZE_ATOM)));
        processInfo.setStackSize(toLong(propList.get(STACK_SIZE_ATOM)));
        processInfo.setTotalHeapSize(toLong(propList.get(TOTAL_HEAP_SIZE_ATOM)));

        return processInfo;
    }

    private static long toLong(Object object) {
        if (object instanceof OtpErlangLong) {
            return ((OtpErlangLong) object).longValue();
        }
        return 0;
    }

    /**
     * We shouldn't need to implement this but sometimes {@link TableView}
     * attempts to sort the {@link ProcInfo} objects itself and tries to cast
     * {@link ProcInfo} to {@link Comparable}.
     * <p>
     * To avoid this exception we're just implementing comparable even if it
     * gives the wrong sort to what the user expected.
     */
    @Override
    public int compareTo(ProcInfo o) {
        return getProcessName().compareTo(o.getProcessName());
    }

    public String getShortName() {
        String processName2 = getProcessName();
        if(processName2 != null && !"".equals(getProcessName()))
            return getProcessName();
        return getPid();
    }
}

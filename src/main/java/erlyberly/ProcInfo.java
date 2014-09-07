package erlyberly;

import java.util.Map;
import java.util.Objects;

import javafx.beans.property.LongProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ProcInfo {

	private static final OtpErlangAtom REGISTERED_NAME_ATOM = new OtpErlangAtom("registered_name");

	private static final OtpErlangAtom REDUCTIONS_ATOM = new OtpErlangAtom("reductions");

	private StringProperty processName;

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

	private LongProperty reductions;

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

		if (processName == null) {
			OtpErlangPid pid = (OtpErlangPid) propList.get("pid");
			
			// the target node is always zero to itself, I think!
			processName = "<0." + pid.id() + "." + pid.serial() + ">";
		}

		ProcInfo processInfo;
		processInfo = new ProcInfo();
		processInfo.setProcessName(Objects.toString(processName, ""));
		processInfo.setReductions(toLong(propList.get(REDUCTIONS_ATOM)));
		return processInfo;
	}

	private static long toLong(Object object) {
		if (object instanceof OtpErlangLong) {
			return ((OtpErlangLong) object).longValue();
		}
		return 0;
	}
}

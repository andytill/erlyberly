package erlyberly.node;

import java.time.LocalDateTime;

public class AppProcs {
    private final int procCount;

    private final LocalDateTime dateTime;

    public AppProcs(int theProcCount, LocalDateTime theDateTime) {
        procCount = theProcCount;
        dateTime = theDateTime;
    }

    public int getProcCount() {
        return procCount;
    }

    public LocalDateTime getDateTime() {
        return dateTime;
    }
}

package erlyberly;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

class CloseableExecutorService implements AutoCloseable {
    private final ScheduledExecutorService service;

    CloseableExecutorService(final ScheduledExecutorService service) {
        super();
        this.service = service;
    }

    void scheduleAtFixedRate(final Runnable command, final long initialDelay, final long period, final TimeUnit unit) {
        if (null == this.service) {
            return;
        }
        this.service.scheduleAtFixedRate(command, initialDelay, period, unit);
    }

    @Override
    public void close() {
        if (null != this.service) {
            this.service.shutdown();
        }
    }
}
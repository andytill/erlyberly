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

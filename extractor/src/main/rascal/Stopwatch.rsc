module Stopwatch

import DateTime;

tuple[&Return, Duration] runTimed(&Return (&Param) f, &Param param) {
    begin = now();
    result = f(param);
    end = now();
    return <result, createDuration(begin, end)>;
}

Duration runTimedVoid(void (&Param) f, &Param param) {
    begin = now();
    f(param);
    end = now();
    return createDuration(begin, end);
}

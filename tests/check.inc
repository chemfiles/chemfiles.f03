#define CHECK(_condition_) if (.not. _condition_) then;                             \
    write(*, *) "assertion '_condition_' failed at ", __FILE__, " line ", __LINE__; \
    stop 1;                                                                         \
end if

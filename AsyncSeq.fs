module AsyncSeq

type AsyncSeq<'a> = {
    value: Async<'a>
}

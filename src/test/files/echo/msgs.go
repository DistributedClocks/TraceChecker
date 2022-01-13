package echo

import "github.com/DistributedClocks/tracing"

type RequestMsg struct {
	Token tracing.TracingToken
	Kill bool
}

type ResponseMsg struct {
	Token tracing.TracingToken
}

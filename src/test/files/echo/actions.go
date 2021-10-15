package echo

type ServerStart struct {}
type ServerStop struct {}

type ServerEcho struct {
	Kill bool
}

type ClientStart struct {
	Kill bool
	RequestCount int
}
type ClientStop struct {}

type ClientSend struct {}
type ClientReceive struct {}



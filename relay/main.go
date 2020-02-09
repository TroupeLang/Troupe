package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"time"

	golog "github.com/ipfs/go-log"
	"github.com/libp2p/go-libp2p"
	connmgr "github.com/libp2p/go-libp2p-connmgr"
	"github.com/libp2p/go-libp2p-core/crypto"
	"github.com/libp2p/go-libp2p-core/network"
	"github.com/libp2p/go-libp2p-core/peer"

	// dht "github.com/libp2p/go-libp2p-kad-dht"
	// "github.com/libp2p/go-libp2p-core/host"
	// routing "github.com/libp2p/go-libp2p-routing"

	mplex "github.com/libp2p/go-libp2p-mplex"
	secio "github.com/libp2p/go-libp2p-secio"
	"github.com/multiformats/go-multiaddr"

	circuit "github.com/libp2p/go-libp2p-circuit"
	swarm "github.com/libp2p/go-libp2p-swarm"
	ma "github.com/multiformats/go-multiaddr"
)

func main() {
	golog.SetLogLevel("*", "info")
	keyfile := flag.String("key", "", "key file")
	flag.Parse()

	// Create three libp2p hosts, enable relay client capabilities on all
	// of them.

	// Tell the host to monitor for relays.
	h1, err := libp2p.New(context.Background(), libp2p.EnableRelay(circuit.OptDiscovery))
	if err != nil {
		panic(err)
	}
	var prvKey crypto.PrivKey
	// Tell the host to relay connections for other peers (The ability to *use*
	// a relay vs the ability to *be* a relay)
	if *keyfile == "" {
		// panic(errors.New("no keyfile provided"))
		prvKey, _, err = crypto.GenerateKeyPair(crypto.RSA, 2048)
	} else {
		var decoded []byte
		pubBytes, err := ioutil.ReadFile(*keyfile)
		if err != nil {
			panic(err)
		}
		text := string(pubBytes)
		decoded, err = crypto.ConfigDecodeKey(text)
		if err != nil {
			panic(err)
		}
		prvKey, err = crypto.UnmarshalPrivateKey(decoded)
		if err != nil {
			panic(err)
		}
	}

	sourceMultiAddr, _ := multiaddr.NewMultiaddr(fmt.Sprintf("/ip4/0.0.0.0/tcp/%d", 5555))

	// var idht *dht.IpfsDHT
	ctx := context.Background()
	h2, err := libp2p.New(ctx,
		libp2p.Identity(prvKey),
		libp2p.ListenAddrs(sourceMultiAddr),
		libp2p.Muxer("/mplex/6.7.0", mplex.DefaultTransport),
		// support secio connections
		libp2p.Security(secio.ID, secio.New),
		// support any other default transports (TCP)
		libp2p.DefaultTransports,
		// Let this host use the DHT to find other hosts
		/*
			libp2p.Routing(func(h host.Host) (routing.PeerRouting, error) {
				idht, err = dht.New(ctx, h)
				return idht, err
			}),
		*/
		libp2p.EnableRelay(circuit.OptHop),
		libp2p.ConnectionManager(connmgr.NewConnManager(30, 100, time.Microsecond*5000)),
		// libp2p.EnableAutoRelay()
	)
	if err != nil {
		panic(err)
	}

	h2.SetStreamHandler("/trouperelay/keepalive", func(s network.Stream) {
		fmt.Printf("Got a new stream keepalive stream!\n")
		go handleKeepAlive(s)

	})

	// Zero out the listen addresses for the host, so it can only communicate
	// via p2p-circuit for our example

	h3, err := libp2p.New(context.Background(), libp2p.ListenAddrs(), libp2p.EnableRelay())
	if err != nil {
		panic(err)
	}

	h2info := peer.AddrInfo{
		ID:    h2.ID(),
		Addrs: h2.Addrs(),
	}

	// Connect both h1 and h3 to h2, but not to each other
	if err := h1.Connect(context.Background(), h2info); err != nil {
		panic(err)
	}
	if err := h3.Connect(context.Background(), h2info); err != nil {
		panic(err)
	}

	// Now, to test things, let's set up a protocol handler on h3
	h3.SetStreamHandler("/cats", func(s network.Stream) {
		fmt.Println("Meow! It worked!")
		s.Close()
	})

	_, err = h1.NewStream(context.Background(), h3.ID(), "/cats")
	if err == nil {
		fmt.Println("Didnt actually expect to get a stream here. What happened?")
		return
	}
	fmt.Println("Okay, no connection from h1 to h3: ", err)
	fmt.Println("Just as we suspected")

	// Creates a relay address
	relayaddr, err := ma.NewMultiaddr("/p2p-circuit/p2p/" + h3.ID().Pretty())
	if err != nil {
		panic(err)
	}

	// Since we just tried and failed to dial, the dialer system will, by default
	// prevent us from redialing again so quickly. Since we know what we're doing, we
	// can use this ugly hack (it's on our TODO list to make it a little cleaner)
	// to tell the dialer "no, its okay, let's try this again"
	h1.Network().(*swarm.Swarm).Backoff().Clear(h3.ID())

	h3relayInfo := peer.AddrInfo{
		ID:    h3.ID(),
		Addrs: []ma.Multiaddr{relayaddr},
	}
	if err := h1.Connect(context.Background(), h3relayInfo); err != nil {
		panic(err)
	}

	// Woohoo! we're connected!
	s, err := h1.NewStream(context.Background(), h3.ID(), "/cats")
	if err != nil {
		fmt.Println("huh, this should have worked: ", err)
		return
	}

	s.Read(make([]byte, 1)) // block until the handler closes the stream

	// Hang forever.
	select {}
}

func handleKeepAlive(s network.Stream) error {
	greeting := "troupe p2p go relay"
	fmt.Println("handling the keep alive connection")
	rw := bufio.NewReadWriter(bufio.NewReader(s), bufio.NewWriter(s))
	for {
		str, err := rw.ReadString('\n')
		if err != nil {
			log.Println(err)
			s.Reset()
			return err
		}
		fmt.Print(str)
		_, err = rw.WriteString(greeting + "/" + str)
		rw.Flush()
		if err != nil {
			log.Println(err)
			s.Reset()
			return err
		}
	}
}

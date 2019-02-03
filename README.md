# spikes-utils
Utilities for parsing and describing spikes neural networks. These classes can be used by your application to 
parse a spikes neuron network into a `NetworkDescription`. The the core `spikes` application uses its `NetworkBuilder` 
to construct the distributed neural network based on the instructions in your `NetworkDescription`.

## spikes project
An event-based quasi-real-time spiking neural network. Spikes attempts to model neural network dynamics, that are reasonably
grounded biology, based on the notion that learning, memory, and thought emerge from dynamical processes
determined by the structure and behavior of a collection of neurons that are continuously active.

To accomplish this, **Spikes** adds time and geometry to the topology of neural networks. In **Spikes**, the geometry of the network
determines signal propagation times, and time effects the decay of the neurons' membrane potential, the decay of their connection weights,
and the adjustment of connection weights based on signal arrival time relative to pre-synaptic spikes (learning, spike 
time-dependent plasticity). 

The arrival of a signal from a pre-synaptic neuron, or the environment, is an *event*. Events have an
associated time. The time of the event causes the neuron to recalculate its state relative the last event, and then
update itself based on the event.

These class are used for parsing the `*.boo` files and describing the network. 

## parsing
Parser that takes a network description file (e.g the `.boo` files) and converts them into a `NetworkDescription` that 
can be used by the core spikes application to construct a `spikes` artificial neural network. Below is an example
of a simple winner-takes-all spikes neural network.
```text
// line sensor network
// For parameters that accept units, if they are not specified, they default to:
// • distances to µm
// • times to ms
// • conductance speeds to m/s
// • electric potentials to mV
// • frequencies to Hz
// • magnetic flux to Wb
// notes
// • wnm from 1e-3 to 0
// • ipl from 0.001 to 0.00 for output layer
// • mpn from 0.05 to 0.0
(
GRP=[
    (gid=group1)
],
NRN=[
    // input layer
    (nid=in-1, grp=group1, nty=mi, mst=1 mV, inh=f, rfp=2 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=1.1 mV, csp=0.1 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=0.1, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=-300 µm, px2=0µm, px3=100 µm)
    ),
    (nid=in-2, grp=group1, nty=mi, mst=1 mV, inh=f, rfp=2 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=1.1 mV, csp=0.1 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=0.1, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=300 µm, px2=0 µm, px3=100 µm)
    ),

    // inhibition neuron
    (nid=inh-1, grp=group1, nty=mi, mst=0.4 mV, inh=t, rfp=0.1 ms, rfb=0.1 µWb, mnp=0 mV, mpd=250 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=0.5 mV, csp=0.08 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=exp, dhl=10 s),
        SRP=(fcb=1000, fcm=0, fct=100 ms, dpb=1000, dpm=0, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.5),
        LOC=(cst=ct, px1=-290 µm, px2=0 µm, px3=0 µm)
    ),
    (nid=inh-2, grp=group1, nty=mi, mst=0.4 mV, inh=t, rfp=0.1 ms, rfb=0.1 µWb, mnp=0 mV, mpd=250 ms, mpr=2 ms, mpn=0.0 mV, wnm=0, spp=0.5 mV, csp=0.08 m/s,
        ipb=0 mV, ipl=0 mV, ipd=3600 s,
        WDF=(fnc=exp, dhl=10 s),
        SRP=(fcb=1000, fcm=0, fct=100 ms, dpb=1000, dpm=0, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.5),
        LOC=(cst=ct, px1=290 µm, px2=0 µm, px3=0 µm)
    ),

    // output layer
    (nid=out-1, grp=group1, nty=mi, mst=1.0 mV, inh=f, rfp=20 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=1e-5, spp=1 mV, csp=1 m/s,
        ipb=0 mV, ipl=0 nV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=10, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=-300 µm, px2=0 µm, px3=0 µm)
    ),
    (nid=out-2, grp=group1, nty=mi, mst=1.0 mV, inh=f, rfp=20 ms, rfb=0.1 µWb, mnp=0 mV, mpd=2500 ms, mpr=2 ms, mpn=0.0 mV, wnm=1e-5, spp=1 mV, csp=1 m/s,
        ipb=0 mV, ipl=0 nV, ipd=3600 s,
        WDF=(fnc=zer),
        SRP=(fcb=1000, fcm=0.1, fct=100 ms, dpb=1000, dpm=10, dpt=100 ms),
        WLF=(fnc=bnd, lwb=0.0, upb=1.0),
        LOC=(cst=ct, px1=300 µm, px2=0 µm, px3=0 µm)
    )
],

CON=[
    // input to output
    (prn=in-{1,2}, psn=out-{1,2}, cnw=0.5, eqw=0.5, lrn=stdp_alpha),

    // output to inhibition
    (prn=out-1, psn=inh-1, cnw=1, eqw=1, lrn=flat),
    (prn=out-2, psn=inh-2, cnw=1, eqw=1, lrn=flat),

    // inhib to output
    (prn=inh-1, psn=out-2, cnw=1, eqw=1, lrn=flat),
    (prn=inh-2, psn=out-1, cnw=1, eqw=1, lrn=flat)
],

LRN=[
    (fnc=stdp_soft, ina=0.06, inp=15 ms, exa=0.02, exp=10 ms),
    (fnc=stdp_alpha, bln=-1, alr=0.04, atc=22 ms),
    (fnc=flat)
]
)
```

## coordinates
The coordinates classes provide a spatial (and dimensionless) coordinate system for describing neural
network topology. To account for different symmetries, and organizational structures, a neuron's coordinate
can be defined using cartesian, cylindrical, and spherical coordinate systems. And to facilitate mixing 
coordinate systems, conversion between coordinate types are handled using implicits. For example, With 
`spikes-coords` you can add or subtract cartesian and cylindrical coordinates. For example,

```scala
import com.digitalcipher.spiked.topology.coords.dimensionless._

val origin = Cartesian(1, 0, 0) - Cylindrical(1, 0, 0)
```

yields the cartesian origin `Cartesian(0, 0, 0)`.

The `spikes-coord` library also provides spatial coordinate systems. These are coordinate systems with
a measure of distance attached. The spatial units default to µm for distance, which are biologically 
sensible for describing neuronal locations. The angle default to radians. In this way

```scala
import com.digitalcipher.spiked.topology.coords.spatial._
import squants.space._

Cylindrical(1, math.Pi / 2, 0) should equal(Cylindrical(Microns(1), Radians(math.Pi / 2), Microns(0)))
```

## coordinate systems

The three types of coordinate systems for spatial and dimensionless coordinates offer by the `spikes-coords` 
library are:
1. Cartesian (x, y, z)
2. Cylindrical (r, φ, z)
3. Spherical (r, φ, θ)

## conversions
There are a set of functions that allow conversion between coordinate systems. 

| function | arguments | description |
| -------- | --- | --- |
| `toCartesian(...)` | `Cylindrical` or `Spherical` | Converts cylindrical and spherical coordinates to cartesian coordinates |
| `to Cylindrical(...)` | `Cartesian` or `Spherical` | Converts cartesian and spherical coordinates to cylindrical coordinates |
| `toSpherical(...)` | `Cartesian` or `Spherical` | Converts cartesian and cylindrical coordinates to spherical coordinates |

The conversion functions for spatial coordinate systems is in the 
[`com.digitalcipher.spiked.topology.coords.spatial`](src/main/scala/com/digitalcipher/spiked/topology/coords/spatial/package.scala) 
package object. And the conversion functions for dimensionless coordinates are in the 
[`com.digitalcipher.spiked.topology.coords.dimensionless`](src/main/scala/com/digitalcipher/spiked/topology/coords/dimensionless/package.scala)
package object. 
 
For example, to convert a spatial cylindrical coordinate to a cartesian coordinate
```scala
import com.digitalcipher.spiked.topology.coords.spatial

toCartesian(Cylindrical(1, 0, 1)) should equal(Cartesian(1, 0, 1))
```

or using `squants` natural language DSL
```scala
import scala.language.postfixOps
import squants.space.LengthConversions._
import squants.space.AngleConversions._

import com.digitalcipher.spiked.topology.coords.spatial

toCartesian(Cylindrical(1 µm, 0 radians, 1 µm)) should equal(Cartesian(1 µm, 0 µm, 1 µm))
toCartesian(Cylindrical(1 µm, math.Pi / 2 radians, 0 µm)) should equal(Cartesian(0 µm, 1 µm, 0 µm))
```

## distance and norm
All the coordinates provide a `norm` method, which is the distance to the origin. For example,
```scala
Cartesian(1, 0, 0).norm should be (1)
```

And with the beuauty of `squants` you can write this in a natural way as
```scala
import scala.language.postfixOps
import squants.space.LengthConversions._
import squants.space.AngleConversions._

Spherical(1 µm, 0 radians, 0 radians).norm should be (1 µm)
```

Coordinates also provide a method to calculate the distance to another coordinate. For example,
to calculate the distance between two dimensionless coordinates.
```scala
Cartesian(1, 0, 0).distanceTo(Cartesian(2, 0, 0)) should be (1)
```
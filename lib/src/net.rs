use crate::internal::*;
use std::net;

macro_rules! address {
    ($($fragment:ident)::*, $ty:expr) => {
        impl FromArg for $($fragment)::* {
            type Parent = Box<str>;
            type Error = ();

            fn from_arg(arg: Self::Parent) -> Result<Self, Self::Error> {
                arg.parse().map_err(|_| ())
            }

            fn box_error((): Self::Error) -> Box<dyn Error + Send + Sync> {
                Box::new(TypeError($ty))
            }
        }
    };
}

address!(net::Ipv4Addr, "ip-v4 address");
address!(net::Ipv6Addr, "ip-v6 address");
address!(net::IpAddr, "ip address");
address!(net::SocketAddrV4, "v4 socket");
address!(net::SocketAddrV6, "v6 socket");
address!(net::SocketAddr, "socket");

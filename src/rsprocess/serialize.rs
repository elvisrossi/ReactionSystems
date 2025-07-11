//! Definitions for serializing and deserializing graph and translator.
//!
//! N.B. after serialization the size of the graph may be much larger than
//! before since a lot of ```Rc``` are used in ```RSsystem```.

use std::io;

use petgraph::Graph;
use serde::{Deserialize, Serialize};

use super::{structure::{RSlabel, RSsystem},
	    translator::Translator};

#[derive(Serialize, Deserialize)]
struct GraphAndTranslator {
    graph: Graph<RSsystem, RSlabel>,
    translator: Translator
}

/// Serializer for graph and translator.
pub fn ser<W>(
    writer: W,
    graph: &Graph<RSsystem, RSlabel>,
    translator: &Translator
) -> Result<(), serde_cbor_2::Error>
where
    W: io::Write,
{
    serde_cbor_2::to_writer(writer,
			    &GraphAndTranslator {
				graph: graph.clone(),
				translator: translator.clone()
			    })
}

/// Deserializer for file that contains graph and translator.
pub fn de<R>(
    reader: R
) -> Result<(Graph<RSsystem, RSlabel>, Translator), serde_cbor_2::Error>
where
    R: io::Read,
{
    let gat: GraphAndTranslator = serde_cbor_2::from_reader(reader)?;
    Ok((gat.graph, gat.translator))
}

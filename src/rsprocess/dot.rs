//! Slightly modified Simple graphviz dot file format output.
//! See petgraph::dot::mod.

static PRINTNAMES: bool = false;

use core::fmt::{self, Display, Write};
use petgraph::{
    data::DataMap,
    visit::{
        EdgeRef, GraphProp, IntoEdgeReferences, IntoNodeReferences,
        NodeIndexable, NodeRef,
    },
};

pub struct Dot<'a, G>
where
    G: IntoEdgeReferences + IntoNodeReferences + DataMap,
{
    graph: G,
    get_edge_attributes: &'a dyn Fn(G, G::EdgeRef) -> String,
    get_node_attributes: &'a dyn Fn(G, G::NodeRef) -> String,
    config: Configs,
}

static TYPE: [&str; 2] = ["graph", "digraph"];
static EDGE: [&str; 2] = ["--", "->"];
static INDENT: &str = "    ";

impl<'a, G> Dot<'a, G>
where
    G: IntoNodeReferences + IntoEdgeReferences + DataMap,
{
    /// Create a `Dot` formatting wrapper with default configuration.
    #[inline]
    pub fn new(graph: G) -> Self {
        Dot {
            graph,
            get_edge_attributes: &|_, _| String::new(),
            get_node_attributes: &|_, _| String::new(),
            config: Configs::default(),
        }
    }

    /// Create a `Dot` formatting wrapper with custom configuration.
    #[inline]
    pub fn with_config(graph: G, config: &'a [Config]) -> Self {
        let config = Configs::extract(config);
        Dot {
            graph,
            get_edge_attributes: &|_, _| String::new(),
            get_node_attributes: &|_, _| String::new(),
            config,
        }
    }

    #[inline]
    pub fn with_attr_getters(
        graph: G,
        config: &'a [Config],
        get_edge_attributes: &'a dyn Fn(G, G::EdgeRef) -> String,
        get_node_attributes: &'a dyn Fn(G, G::NodeRef) -> String,
    ) -> Self {
        let config = Configs::extract(config);
        Dot {
            graph,
            get_edge_attributes,
            get_node_attributes,
            config,
        }
    }
}

/// Direction of graph layout.
///
/// <https://graphviz.org/docs/attrs/rankdir/>
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RankDir {
    /// Top to bottom
    #[default]
    TB,
    /// Bottom to top
    BT,
    /// Left to right
    LR,
    /// Right to left
    RL,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeStyle {
    shape: String,
    style: String,
}

impl Default for NodeStyle {
    fn default() -> Self {
        NodeStyle {
            shape: "box".to_string(),
            style: "filled, rounded".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdgeStyle {
    arrowhead: String,
}

impl Default for EdgeStyle {
    fn default() -> Self {
        EdgeStyle {
            arrowhead: "vee".to_string(),
        }
    }
}

/// `Dot` configuration.
///
/// This enum does not have an exhaustive definition (will be expanded)
#[derive(Debug, PartialEq, Eq)]
pub enum Config {
    /// Sets direction of graph layout.
    RankDir(RankDir),
    /// Node style
    NodeStyle(NodeStyle),
    /// Edge style
    EdgeStyle(EdgeStyle),
}
macro_rules! make_config_struct {
    ($($variant:ident,)*) => {
	#[allow(non_snake_case)]
	struct Configs {
	    $($variant: bool,)*
	    RankDir: Option<RankDir>,
	    NodeStyle: Option<NodeStyle>,
	    EdgeStyle: Option<EdgeStyle>,
	}
	impl Configs {
	    #[inline]
	    fn extract(configs: &[Config]) -> Self {
		let mut conf = Self::default();
		for c in configs {
		    match c {
			$(Config::$variant => conf.$variant = true,)*
			Config::RankDir(dir) => conf.RankDir = Some(*dir),
			Config::NodeStyle(style) =>
			    conf.NodeStyle = Some(style.clone()),
			Config::EdgeStyle(style) =>
			    conf.EdgeStyle = Some(style.clone()),
		    }
		}
		conf
	    }
	}
	impl Default for Configs {
	    fn default() -> Self {
		Configs {
		    $(Config::$variant: true,)*
		    RankDir: Some(RankDir::default()),
		    NodeStyle: Some(NodeStyle::default()),
		    EdgeStyle: Some(EdgeStyle::default()),
		}
	    }
	}
    }
}

make_config_struct!();

impl<G> Dot<'_, G>
where
    G: IntoNodeReferences
        + IntoEdgeReferences
        + NodeIndexable
        + GraphProp
        + DataMap,
{
    fn graph_fmt<NF, EF>(
        &self,
        f: &mut fmt::Formatter,
        node_fmt: NF,
        edge_fmt: EF,
    ) -> fmt::Result
    where
        NF: Fn(&G::NodeWeight, &mut fmt::Formatter) -> fmt::Result,
        EF: Fn(&G::EdgeWeight, &mut fmt::Formatter) -> fmt::Result,
    {
        let g = self.graph;
        writeln!(f, "{} {{", TYPE[g.is_directed() as usize])?;

        if let Some(rank_dir) = &self.config.RankDir {
            let value = match rank_dir {
                RankDir::TB => "TB",
                RankDir::BT => "BT",
                RankDir::LR => "LR",
                RankDir::RL => "RL",
            };
            writeln!(f, "{INDENT}rankdir=\"{value}\"\n")?;
        }

        if let Some(style) = &self.config.NodeStyle {
            writeln!(
                f,
                "{INDENT}node [shape=\"{}\", style=\"{}\"]",
                style.shape, style.style
            )?;
        }

        if let Some(style) = &self.config.EdgeStyle {
            writeln!(f, "{INDENT}edge [arrowhead=\"{}\"]\n", style.arrowhead)?;
        }

        // output all labels
        for node in g.node_references() {
            if PRINTNAMES {
                write!(f, "{INDENT}\"")?;
                Escaped(FnFmt(node.weight(), &node_fmt)).fmt(f)?;
                write!(f, "\" [ ")?;
            } else {
                write!(f, "{INDENT}")?;
                write!(f, "{}", g.to_index(node.id()))?;
                write!(f, " [ ")?;
            }

            write!(f, "label = \"")?;
            Escaped(FnFmt(node.weight(), &node_fmt)).fmt(f)?;
            write!(f, "\" ")?;

            writeln!(f, "{}]", (self.get_node_attributes)(g, node))?;
        }
        // output all edges
        for edge in g.edge_references() {
            if PRINTNAMES {
                write!(f, "{INDENT}\"")?;
                let node_source_weight = g.node_weight(edge.source()).unwrap();
                Escaped(FnFmt(node_source_weight, &node_fmt)).fmt(f)?;
                write!(f, "\" {} \"", EDGE[g.is_directed() as usize])?;
                let node_target_weight = g.node_weight(edge.target()).unwrap();
                Escaped(FnFmt(node_target_weight, &node_fmt)).fmt(f)?;
                write!(f, "\" [ ")?;
            } else {
                write!(f, "{INDENT}")?;
                write!(f, "{} ", g.to_index(edge.source()))?;
                write!(f, "{} ", EDGE[g.is_directed() as usize])?;
                write!(f, "{} ", g.to_index(edge.target()))?;
                write!(f, "[ ")?;
            }

            write!(f, "label = \"")?;
            Escaped(FnFmt(edge.weight(), &edge_fmt)).fmt(f)?;
            write!(f, "\" ")?;

            writeln!(f, "{}]", (self.get_edge_attributes)(g, edge))?;
        }

        writeln!(f, "}}")?;

        Ok(())
    }
}

impl<G> fmt::Display for Dot<'_, G>
where
    G: IntoEdgeReferences
        + IntoNodeReferences
        + NodeIndexable
        + GraphProp
        + DataMap,
    G::EdgeWeight: fmt::Display,
    G::NodeWeight: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.graph_fmt(f, fmt::Display::fmt, fmt::Display::fmt)
    }
}

impl<G> fmt::LowerHex for Dot<'_, G>
where
    G: IntoEdgeReferences
        + IntoNodeReferences
        + NodeIndexable
        + GraphProp
        + DataMap,
    G::EdgeWeight: fmt::LowerHex,
    G::NodeWeight: fmt::LowerHex,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.graph_fmt(f, fmt::LowerHex::fmt, fmt::LowerHex::fmt)
    }
}

impl<G> fmt::UpperHex for Dot<'_, G>
where
    G: IntoEdgeReferences
        + IntoNodeReferences
        + NodeIndexable
        + GraphProp
        + DataMap,
    G::EdgeWeight: fmt::UpperHex,
    G::NodeWeight: fmt::UpperHex,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.graph_fmt(f, fmt::UpperHex::fmt, fmt::UpperHex::fmt)
    }
}

impl<G> fmt::Debug for Dot<'_, G>
where
    G: IntoEdgeReferences
        + IntoNodeReferences
        + NodeIndexable
        + GraphProp
        + DataMap,
    G::EdgeWeight: fmt::Debug,
    G::NodeWeight: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.graph_fmt(f, fmt::Debug::fmt, fmt::Debug::fmt)
    }
}

/// Escape for Graphviz
struct Escaper<W>(W);

impl<W> fmt::Write for Escaper<W>
where
    W: fmt::Write,
{
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.chars() {
            self.write_char(c)?;
        }
        Ok(())
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        match c {
            '"' | '\\' => self.0.write_char('\\')?,
            // \l is for left justified linebreak
            '\n' => return self.0.write_str("\\l"),
            _ => {}
        }
        self.0.write_char(c)
    }
}

/// Pass Display formatting through a simple escaping filter
struct Escaped<T>(T);

impl<T> fmt::Display for Escaped<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            writeln!(&mut Escaper(f), "{:#}", &self.0)
        } else {
            write!(&mut Escaper(f), "{}", &self.0)
        }
    }
}

/// Format data using a specific format function
struct FnFmt<'a, T, F>(&'a T, F);

impl<'a, T, F> fmt::Display for FnFmt<'a, T, F>
where
    F: Fn(&'a T, &mut fmt::Formatter<'_>) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.1(self.0, f)
    }
}

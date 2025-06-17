// translate and keeps track of strings

use std::collections::HashMap;

pub type IdType = u32;

#[derive(Clone, Debug)]
pub struct Translator {
    strings: HashMap<String, IdType>,
    last_id: IdType
}

impl Translator {
    pub fn new() -> Self {
	Translator { strings: HashMap::new(), last_id: 0 }
    }


}

impl Translator {
    pub fn convert(&mut self, s: impl Into<String>) -> IdType {
	*(self.strings.entry(s.into()).or_insert(
	    {self.last_id += 1;
	     self.last_id
	    }
	))
    }
}

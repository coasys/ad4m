use hdk::prelude::*;
use perspective_diff_sync_integrity::{
    EntryTypes, LinkExpression, PerspectiveDiff,
};

use crate::{Hash, CHUNK_SIZE};
use crate::errors::{SocialContextResult};
use crate::retriever::{PerspectiveDiffRetreiver};

#[derive(Clone)]
pub struct ChunkedDiffs {
    max_changes_per_chunk: u16,
    pub chunks: Vec<PerspectiveDiff>
}

impl ChunkedDiffs {
    pub fn new(max: u16) -> Self {
        Self {
            max_changes_per_chunk: max,
            chunks: vec![PerspectiveDiff::new()],
        }
    }

    pub fn add_additions(&mut self, links: Vec<LinkExpression>) {
        let mut reverse_links = links.into_iter().rev().collect::<Vec<_>>();
        while reverse_links.len() > 0 {
            let len = self.chunks.len();
            let current_chunk = self.chunks.get_mut(len-1).expect("must have at least one");
    
            while current_chunk.total_diff_number() < self.max_changes_per_chunk.into() && reverse_links.len() > 0 {
                current_chunk.additions.push(reverse_links.pop().unwrap());
            }
    
            if reverse_links.len() > 0 {
                self.chunks.push(PerspectiveDiff::new())
            }
        }
    }

    pub fn add_removals(&mut self, links: Vec<LinkExpression>) {
        let mut reverse_links = links.into_iter().rev().collect::<Vec<_>>();
        while reverse_links.len() > 0 {
            let len = self.chunks.len();
            let current_chunk = self.chunks.get_mut(len-1).expect("must have at least one");

            while current_chunk.total_diff_number() < self.max_changes_per_chunk.into() && reverse_links.len() > 0 {
                current_chunk.removals.push(reverse_links.pop().unwrap());
            }
    
            if reverse_links.len() > 0 {
                self.chunks.push(PerspectiveDiff::new())
            }
        }
    }

    pub fn into_entries<Retreiver: PerspectiveDiffRetreiver>(self) -> SocialContextResult<Vec<Hash>> {
        debug!("ChunkedDiffs.into_entries()");
        self.chunks
            .into_iter()
            .map(|chunk_diff| {
                debug!("ChunkedDiffs writing chunk of size: {}", chunk_diff.total_diff_number());
                Retreiver::create_entry(EntryTypes::PerspectiveDiff(chunk_diff))
            })
            .collect() 
    }

    pub fn from_entries<Retreiver: PerspectiveDiffRetreiver>(hashes: Vec<Hash>) -> SocialContextResult<Self> {
        let mut diffs = Vec::new();
        for hash in hashes.into_iter() {
            diffs.push(Retreiver::get::<PerspectiveDiff>(hash)?);
        }

        Ok(ChunkedDiffs {
            max_changes_per_chunk: *CHUNK_SIZE,
            chunks: diffs,
        })
    }

    pub fn into_aggregated_diff(self) -> PerspectiveDiff {
        self.chunks.into_iter().reduce(|accum, item| {
            let mut temp = accum.clone();
            temp.additions.append(&mut item.additions.clone());
            temp.removals.append(&mut item.removals.clone());
            temp
        })
        .unwrap_or(PerspectiveDiff::new())
    }
}


#[cfg(test)]
mod tests {
    use super::ChunkedDiffs;
    use crate::utils::create_link_expression;
    use crate::retriever::{GLOBAL_MOCKED_GRAPH, MockPerspectiveGraph};

    #[test]
    fn can_chunk() {
        let mut chunks = ChunkedDiffs::new(5);

        chunks.add_additions(vec![
            create_link_expression("a", "1"),
            create_link_expression("a", "2"),
            create_link_expression("a", "3"),
        ]);

        assert_eq!(chunks.chunks.len(), 1);

        chunks.add_additions(vec![
            create_link_expression("a", "4"),
            create_link_expression("a", "5"),
            create_link_expression("a", "6"),
        ]);

        assert_eq!(chunks.chunks.len(), 2);

        chunks.add_removals(vec![
            create_link_expression("a", "1"),
            create_link_expression("a", "2"),
            create_link_expression("a", "3"),
            create_link_expression("a", "4"),
            create_link_expression("a", "5"),
            create_link_expression("a", "6"),
        ]);

        assert_eq!(chunks.chunks.len(), 3);
    }

    #[test]
    fn can_aggregate() {
        let mut chunks = ChunkedDiffs::new(5);

        let _a1 = create_link_expression("a", "1");
        let _a2 = create_link_expression("a", "2");
        let _r1 = create_link_expression("r", "1");
        let _r2 = create_link_expression("r", "2");
        let _r3 = create_link_expression("r", "3");
        let _r4 = create_link_expression("r", "4");


        chunks.add_additions(vec![_a1.clone()]);
        chunks.add_additions(vec![_a2.clone()]);
        chunks.add_removals(vec![_r1.clone(),_r2.clone(),_r3.clone(),_r4.clone()]);

        assert_eq!(chunks.chunks.len(), 2);

        let diff = chunks.into_aggregated_diff();

        assert_eq!(diff.additions, vec![_a1,_a2]);
        assert_eq!(diff.removals, vec![_r1,_r2,_r3,_r4]);
    }

    #[test]
    fn can_chunk_big_diffs() {
        let mut chunks = ChunkedDiffs::new(500);

        let mut big_diff_add = Vec::new();
        for i in 0..5000 {
            big_diff_add.push(create_link_expression("a", &format!("{}", i)));
        }
        chunks.add_additions(big_diff_add);

        let mut big_diff_remove = Vec::new();
        for i in 0..800 {
            big_diff_remove.push(create_link_expression("a", &format!("{}", i)));
        }
        chunks.add_removals(big_diff_remove);

        let mut big_diff_add = Vec::new();
        for i in 0..213 {
            big_diff_add.push(create_link_expression("a", &format!("{}", i)));
        }
        chunks.add_additions(big_diff_add);

        assert_eq!(chunks.chunks.len(), 13);
        for i in 0..12 {
            assert_eq!(chunks.chunks[i].total_diff_number(), 500);
        }
        assert_eq!(chunks.chunks[12].total_diff_number(), 13);
    }

    #[test]
    fn can_write_and_read_entries() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot("digraph{}").expect("can create mock graph from empty dot");
        }
        update();

        let mut chunks = ChunkedDiffs::new(500);

        let mut big_diff_add = Vec::new();
        for i in 0..5000 {
            big_diff_add.push(create_link_expression("a", &format!("{}", i)));
        }
        chunks.add_additions(big_diff_add);

        assert_eq!(chunks.chunks.len(), 10);

        let chunks_clone = chunks.clone();
        let hashes = chunks.into_entries::<MockPerspectiveGraph>().expect("into_entries does not error");
        let read_chunks = ChunkedDiffs::from_entries::<MockPerspectiveGraph>(hashes).expect("from_entries does not error");

        assert_eq!(read_chunks.chunks.len(), 10);
        assert_eq!(format!("{:?}", read_chunks.chunks), format!("{:?}", chunks_clone.chunks));
    }
}

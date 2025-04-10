use std::collections::HashMap;
use uuid::Uuid;

#[derive(Debug, Default)]
pub struct EmbeddingCache {
    // Maps short IDs to full embedding vector URLs
    id_to_vector: HashMap<String, String>,
    // Maps full embedding vector URLs to short IDs
    vector_to_id: HashMap<String, String>,
}

impl EmbeddingCache {
    pub fn new() -> Self {
        Self {
            id_to_vector: HashMap::new(),
            vector_to_id: HashMap::new(),
        }
    }

    pub fn get_or_create_id(&mut self, vector_url: &str) -> String {
        if let Some(id) = self.vector_to_id.get(vector_url) {
            return id.clone();
        }

        // Create new short ID (ev_ prefix makes it clear this is an embedding vector ID)
        let id = format!("ev_{}", Uuid::new_v4().to_string().split('-').next().unwrap());
        
        self.id_to_vector.insert(id.clone(), vector_url.to_string());
        self.vector_to_id.insert(vector_url.to_string(), id.clone());
        
        id
    }

    pub fn get_vector_url(&self, id: &str) -> Option<String> {
        self.id_to_vector.get(id).cloned()
    }

    pub fn _clear(&mut self) {
        self.id_to_vector.clear();
        self.vector_to_id.clear();
    }
} 
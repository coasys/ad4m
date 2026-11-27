//! HDK-side shim onto the algorithm-crate `render` body.

use perspective_diff_algorithm as algo;

use crate::errors::SocialContextResult;
use crate::retriever::PerspectiveDiffRetreiver;
use crate::Perspective;

pub fn render<
    Retriever: PerspectiveDiffRetreiver
        + algo::WorkspaceRetriever
        + algo::RevisionsRetriever
        + algo::PullCommitEnv,
>() -> SocialContextResult<Perspective> {
    Ok(Perspective {
        links: algo::render_perspective_links::<Retriever>()?,
    })
}

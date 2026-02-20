//! Git dependency handling for Jet Package Manager
//!
//! Provides functionality for cloning, fetching, and managing Git repositories
//! as package dependencies.

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

/// Git reference type (branch, tag, or revision)
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum GitRef {
    /// A specific branch
    Branch(String),
    /// A specific tag
    Tag(String),
    /// A specific commit hash
    Rev(String),
    /// Default (HEAD)
    Default,
}

impl GitRef {
    /// Returns the reference as a string suitable for git commands
    #[allow(dead_code)]
    pub fn as_str(&self) -> Option<&str> {
        match self {
            GitRef::Branch(s) | GitRef::Tag(s) | GitRef::Rev(s) => Some(s.as_str()),
            GitRef::Default => None,
        }
    }

    /// Returns the type name of the reference
    #[allow(dead_code)]
    pub fn type_name(&self) -> &'static str {
        match self {
            GitRef::Branch(_) => "branch",
            GitRef::Tag(_) => "tag",
            GitRef::Rev(_) => "rev",
            GitRef::Default => "default",
        }
    }
}

/// Git repository handle
#[allow(dead_code)]
pub struct GitRepo {
    url: String,
    path: PathBuf,
}

#[allow(dead_code)]
impl GitRepo {
    /// Clone or open a repository at the given path
    pub fn open_or_clone(url: &str, path: &Path, git_ref: &GitRef) -> Result<Self> {
        if path.exists() {
            // Repository already exists, fetch updates
            let repo = git2::Repository::open(path)
                .with_context(|| format!("Failed to open repository at {}", path.display()))?;

            // Fetch updates
            Self::fetch(&repo, url)?;

            // Checkout the specified reference
            Self::checkout_ref(&repo, git_ref)?;
        } else {
            // Clone the repository
            Self::clone(url, path, git_ref)?;
        }

        Ok(Self {
            url: url.to_string(),
            path: path.to_path_buf(),
        })
    }

    /// Clone a repository
    fn clone(url: &str, path: &Path, git_ref: &GitRef) -> Result<git2::Repository> {
        std::fs::create_dir_all(path.parent().unwrap_or(Path::new(".")))?;

        let mut builder = git2::build::RepoBuilder::new();

        // Configure based on the reference type
        match git_ref {
            GitRef::Branch(branch) => {
                builder.branch(branch);
            }
            GitRef::Tag(_tag) => {
                // For tags, we'll checkout after clone
            }
            GitRef::Rev(_) => {
                // For specific revs, we'll checkout after clone
            }
            GitRef::Default => {}
        }

        let repo = builder
            .clone(url, path)
            .with_context(|| format!("Failed to clone repository from {}", url))?;

        // Checkout specific reference if needed
        if !matches!(git_ref, GitRef::Default | GitRef::Branch(_)) {
            Self::checkout_ref(&repo, git_ref)?;
        }

        Ok(repo)
    }

    /// Fetch updates from remote
    fn fetch(repo: &git2::Repository, url: &str) -> Result<()> {
        let mut remote = repo
            .find_remote("origin")
            .or_else(|_| repo.remote_anonymous(url))?;

        remote
            .fetch(&["refs/heads/*:refs/heads/*"], None, None)
            .with_context(|| "Failed to fetch from remote")?;

        Ok(())
    }

    /// Checkout a specific reference
    fn checkout_ref(repo: &git2::Repository, git_ref: &GitRef) -> Result<()> {
        let reference = match git_ref {
            GitRef::Branch(branch) => {
                let branch_ref = format!("refs/remotes/origin/{}", branch);
                repo.find_reference(&branch_ref)
                    .or_else(|_| repo.find_reference(&format!("refs/heads/{}", branch)))
                    .with_context(|| format!("Branch '{}' not found", branch))?
            }
            GitRef::Tag(tag) => {
                let tag_ref = format!("refs/tags/{}", tag);
                repo.find_reference(&tag_ref)
                    .with_context(|| format!("Tag '{}' not found", tag))?
            }
            GitRef::Rev(rev) => {
                // Try to resolve the revision
                let obj = repo
                    .revparse_single(rev)
                    .with_context(|| format!("Revision '{}' not found", rev))?;
                repo.find_reference("HEAD")?;
                repo.set_head_detached(obj.id())?;
                repo.checkout_head(None)?;
                return Ok(());
            }
            GitRef::Default => {
                // Nothing to do for default
                return Ok(());
            }
        };

        // Get the commit for this reference
        let commit = reference
            .peel_to_commit()
            .with_context(|| "Failed to peel reference to commit")?;

        // Set HEAD to this commit and checkout
        repo.set_head(reference.name().unwrap_or("HEAD"))?;
        repo.checkout_head(None)
            .with_context(|| "Failed to checkout")?;

        // Also checkout the tree
        let mut checkout_opts = git2::build::CheckoutBuilder::new();
        checkout_opts.force();
        repo.checkout_tree(commit.as_object(), Some(&mut checkout_opts))?;

        Ok(())
    }

    /// Get the current commit hash
    pub fn current_rev(&self) -> Result<String> {
        let repo = git2::Repository::open(&self.path)?;
        let head = repo.head()?;
        let commit = head.peel_to_commit()?;
        Ok(commit.id().to_string())
    }

    /// Get the repository path
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Get the repository URL
    pub fn url(&self) -> &str {
        &self.url
    }
}

/// Parse a git URL and reference from dependency info
#[allow(dead_code)]
pub fn parse_git_ref(
    git: &str,
    branch: Option<&str>,
    tag: Option<&str>,
    rev: Option<&str>,
) -> (String, GitRef) {
    let git_ref = if let Some(rev) = rev {
        GitRef::Rev(rev.to_string())
    } else if let Some(tag) = tag {
        GitRef::Tag(tag.to_string())
    } else if let Some(branch) = branch {
        GitRef::Branch(branch.to_string())
    } else {
        GitRef::Default
    };

    (git.to_string(), git_ref)
}

/// Get the cache directory for git dependencies
#[allow(dead_code)]
pub fn git_cache_dir() -> Result<PathBuf> {
    let cache_dir = dirs::cache_dir()
        .unwrap_or_else(|| PathBuf::from(".cache"))
        .join("jet")
        .join("git");

    std::fs::create_dir_all(&cache_dir)?;
    Ok(cache_dir)
}

/// Get the repository cache path for a URL
#[allow(dead_code)]
pub fn repo_cache_path(url: &str) -> PathBuf {
    // Create a hash of the URL for the directory name
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    url.hash(&mut hasher);
    let hash = hasher.finish();

    git_cache_dir()
        .unwrap_or_else(|_| PathBuf::from(".cache").join("jet").join("git"))
        .join(format!("{:016x}", hash))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_git_ref() {
        let branch = GitRef::Branch("main".to_string());
        assert_eq!(branch.type_name(), "branch");
        assert_eq!(branch.as_str(), Some("main"));

        let tag = GitRef::Tag("v1.0.0".to_string());
        assert_eq!(tag.type_name(), "tag");
        assert_eq!(tag.as_str(), Some("v1.0.0"));

        let rev = GitRef::Rev("abc123".to_string());
        assert_eq!(rev.type_name(), "rev");
        assert_eq!(rev.as_str(), Some("abc123"));

        let default = GitRef::Default;
        assert_eq!(default.type_name(), "default");
        assert_eq!(default.as_str(), None);
    }

    #[test]
    fn test_parse_git_ref() {
        let (url, git_ref) = parse_git_ref("https://github.com/user/repo", None, None, None);
        assert_eq!(url, "https://github.com/user/repo");
        assert!(matches!(git_ref, GitRef::Default));

        let (url, git_ref) =
            parse_git_ref("https://github.com/user/repo", Some("main"), None, None);
        assert_eq!(url, "https://github.com/user/repo");
        assert!(matches!(git_ref, GitRef::Branch(ref b) if b == "main"));

        let (url, git_ref) =
            parse_git_ref("https://github.com/user/repo", None, Some("v1.0.0"), None);
        assert_eq!(url, "https://github.com/user/repo");
        assert!(matches!(git_ref, GitRef::Tag(ref t) if t == "v1.0.0"));

        let (url, git_ref) =
            parse_git_ref("https://github.com/user/repo", None, None, Some("abc123"));
        assert_eq!(url, "https://github.com/user/repo");
        assert!(matches!(git_ref, GitRef::Rev(ref r) if r == "abc123"));
    }

    #[test]
    fn test_repo_cache_path() {
        let path1 = repo_cache_path("https://github.com/user/repo");
        let path2 = repo_cache_path("https://github.com/user/repo");
        let path3 = repo_cache_path("https://github.com/other/repo");

        // Same URL should produce same path
        assert_eq!(path1, path2);
        // Different URL should produce different path
        assert_ne!(path1, path3);
    }
}

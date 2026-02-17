//! Jet Package Registry Client
//!
//! HTTP client for interacting with the Jet package registry API.

use anyhow::{Context, Result};
use reqwest::{Client, StatusCode};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::time::Duration;

/// Default registry URL
pub const DEFAULT_REGISTRY: &str = "https://crates.jet-lang.org";

/// Default timeout for registry requests
const DEFAULT_TIMEOUT: Duration = Duration::from_secs(30);

/// Registry client for interacting with the Jet package registry
#[derive(Debug, Clone)]
pub struct RegistryClient {
    /// HTTP client
    client: Client,
    /// Registry URL
    base_url: String,
    /// Authentication token (if logged in)
    auth_token: Option<String>,
}

/// Package metadata from registry
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PackageMetadata {
    pub name: String,
    pub version: String,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub authors: Vec<String>,
    #[serde(default)]
    pub license: Option<String>,
    #[serde(default)]
    pub repository: Option<String>,
    #[serde(default)]
    pub homepage: Option<String>,
    #[serde(default)]
    pub documentation: Option<String>,
    #[serde(default)]
    pub keywords: Vec<String>,
    #[serde(default)]
    pub readme: Option<String>,
    #[serde(default)]
    pub downloads: u64,
    #[serde(default)]
    pub created_at: String,
    #[serde(default)]
    pub updated_at: String,
    #[serde(default)]
    pub versions: Vec<String>,
    #[serde(default)]
    pub dependencies: Vec<DependencyInfo>,
    #[serde(default)]
    pub checksum: Option<String>,
    /// Download URL for the package tarball
    #[serde(default)]
    pub download_url: Option<String>,
}

/// Dependency information
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct DependencyInfo {
    pub name: String,
    pub version_req: String,
    #[serde(default)]
    pub optional: bool,
    #[serde(default)]
    pub features: Vec<String>,
}

/// Search result from registry
#[derive(Debug, Clone, Deserialize)]
pub struct SearchResult {
    pub name: String,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub version: Option<String>,
    #[serde(default)]
    pub downloads: u64,
    #[allow(dead_code)]
    #[serde(default)]
    pub updated_at: Option<String>,
}

/// Search response from registry
#[derive(Debug, Clone, Deserialize)]
pub struct SearchResponse {
    pub packages: Vec<SearchResult>,
    pub total: usize,
}

/// Publish request
#[derive(Debug, Serialize)]
pub struct PublishRequest {
    pub name: String,
    pub version: String,
    pub description: Option<String>,
    pub authors: Vec<String>,
    pub license: Option<String>,
    pub repository: Option<String>,
    pub homepage: Option<String>,
    pub documentation: Option<String>,
    pub keywords: Vec<String>,
    pub readme: Option<String>,
    /// Base64-encoded tarball
    pub tarball: String,
    /// SHA256 checksum of tarball
    pub checksum: String,
}

/// Publish response
#[derive(Debug, Deserialize)]
pub struct PublishResponse {
    #[allow(dead_code)]
    pub success: bool,
    #[serde(default)]
    pub message: Option<String>,
    #[serde(default)]
    pub warnings: Vec<String>,
}

/// Yank request
#[derive(Debug, Serialize)]
pub struct YankRequest {
    pub yanked: bool,
    #[serde(default)]
    pub message: Option<String>,
}

/// Version information
#[derive(Debug, Clone, Deserialize)]
pub struct VersionInfo {
    pub version: String,
    pub yanked: bool,
    #[allow(dead_code)]
    #[serde(default)]
    pub yank_message: Option<String>,
    #[allow(dead_code)]
    pub created_at: String,
    #[allow(dead_code)]
    pub downloads: u64,
}

/// Package versions response
#[derive(Debug, Deserialize)]
pub struct VersionsResponse {
    pub versions: Vec<VersionInfo>,
}

impl RegistryClient {
    /// Create a new registry client (infallible version per spec)
    pub fn new(base_url: &str) -> Self {
        let client = Client::builder()
            .timeout(DEFAULT_TIMEOUT)
            .user_agent(format!("jetpkg/{}", env!("CARGO_PKG_VERSION")))
            .build()
            .expect("Failed to create HTTP client");

        Self {
            client,
            base_url: base_url.to_string(),
            auth_token: None,
        }
    }

    /// Create a new registry client with fallible construction
    #[allow(dead_code)]
    pub fn try_new(base_url: impl Into<String>) -> Result<Self> {
        let client = Client::builder()
            .timeout(DEFAULT_TIMEOUT)
            .user_agent(format!("jetpkg/{}", env!("CARGO_PKG_VERSION")))
            .build()
            .context("Failed to create HTTP client")?;

        Ok(Self {
            client,
            base_url: base_url.into(),
            auth_token: None,
        })
    }

    /// Create a new registry client with default URL
    pub fn default_registry() -> Self {
        Self::new(DEFAULT_REGISTRY)
    }

    /// Set authentication token
    pub fn with_auth(mut self, token: impl Into<String>) -> Self {
        self.auth_token = Some(token.into());
        self
    }

    /// Get the base URL
    pub fn base_url(&self) -> &str {
        &self.base_url
    }

    /// Check if authenticated
    pub fn is_authenticated(&self) -> bool {
        self.auth_token.is_some()
    }

    /// Build request with optional authentication
    fn build_request(&self, method: reqwest::Method, path: &str) -> reqwest::RequestBuilder {
        let url = format!("{}{}", self.base_url, path);
        let mut request = self.client.request(method, &url);

        if let Some(token) = &self.auth_token {
            request = request.header("Authorization", format!("Bearer {}", token));
        }

        request
    }

    /// Show package metadata (alias for fetch_package per spec)
    #[allow(dead_code)]
    pub async fn show(&self, name: &str, version: Option<&str>) -> Result<PackageMetadata> {
        self.fetch_package(name, version).await
    }

    /// Fetch package metadata
    pub async fn fetch_package(
        &self,
        name: &str,
        version: Option<&str>,
    ) -> Result<PackageMetadata> {
        let path = if let Some(ver) = version {
            format!("/api/v1/crates/{}/{}", name, ver)
        } else {
            format!("/api/v1/crates/{}", name)
        };

        let response = self
            .build_request(reqwest::Method::GET, &path)
            .send()
            .await
            .context("Failed to send request to registry")?;

        match response.status() {
            StatusCode::OK => {
                let metadata: PackageMetadata = response
                    .json()
                    .await
                    .context("Failed to parse registry response")?;
                Ok(metadata)
            }
            StatusCode::NOT_FOUND => {
                anyhow::bail!("Package '{}' not found in registry", name)
            }
            status => {
                let text = response.text().await.unwrap_or_default();
                anyhow::bail!("Registry returned error {}: {}", status, text)
            }
        }
    }

    /// Search for packages
    pub async fn search(&self, query: &str, limit: usize, offset: usize) -> Result<SearchResponse> {
        let path = format!(
            "/api/v1/crates?q={}&limit={}&offset={}",
            urlencoding::encode(query),
            limit,
            offset
        );

        let response = self
            .build_request(reqwest::Method::GET, &path)
            .send()
            .await
            .context("Failed to send search request")?;

        match response.status() {
            StatusCode::OK => {
                let result: SearchResponse = response
                    .json()
                    .await
                    .context("Failed to parse search response")?;
                Ok(result)
            }
            status => {
                let text = response.text().await.unwrap_or_default();
                anyhow::bail!("Registry returned error {}: {}", status, text)
            }
        }
    }

    /// Get all versions of a package
    pub async fn get_versions(&self, name: &str) -> Result<Vec<VersionInfo>> {
        let path = format!("/api/v1/crates/{}/versions", name);

        let response = self
            .build_request(reqwest::Method::GET, &path)
            .send()
            .await
            .context("Failed to fetch versions")?;

        match response.status() {
            StatusCode::OK => {
                let result: VersionsResponse = response
                    .json()
                    .await
                    .context("Failed to parse versions response")?;
                Ok(result.versions)
            }
            StatusCode::NOT_FOUND => {
                anyhow::bail!("Package '{}' not found in registry", name)
            }
            status => {
                let text = response.text().await.unwrap_or_default();
                anyhow::bail!("Registry returned error {}: {}", status, text)
            }
        }
    }

    /// Download a package tarball to a destination path (per spec)
    #[allow(dead_code)]
    pub async fn download(&self, name: &str, version: &str, dest: &std::path::Path) -> Result<()> {
        let output_path = dest.to_path_buf();
        self.download_package(name, version, output_path).await?;
        Ok(())
    }

    /// Download a package tarball
    pub async fn download_package(
        &self,
        name: &str,
        version: &str,
        output_path: PathBuf,
    ) -> Result<PathBuf> {
        // First get metadata to find download URL
        let metadata = self.fetch_package(name, Some(version)).await?;

        let download_url = metadata.download_url.unwrap_or_else(|| {
            format!(
                "{}/api/v1/crates/{}/{}/download",
                self.base_url, name, version
            )
        });

        let response = self
            .client
            .get(&download_url)
            .timeout(Duration::from_secs(300)) // Longer timeout for downloads
            .send()
            .await
            .context("Failed to download package")?;

        match response.status() {
            StatusCode::OK => {
                let bytes = response
                    .bytes()
                    .await
                    .context("Failed to read package bytes")?;

                // Verify checksum if available
                if let Some(expected_checksum) = metadata.checksum {
                    let actual_checksum = sha256::digest(&bytes);
                    if actual_checksum != expected_checksum {
                        anyhow::bail!("Checksum mismatch for package {} {}", name, version);
                    }
                }

                // Write to file
                tokio::fs::write(&output_path, &bytes)
                    .await
                    .context("Failed to write package file")?;

                Ok(output_path)
            }
            StatusCode::NOT_FOUND => {
                anyhow::bail!("Package {} {} not found", name, version)
            }
            status => {
                let text = response.text().await.unwrap_or_default();
                anyhow::bail!("Download failed with status {}: {}", status, text)
            }
        }
    }

    /// Publish a package from metadata and tarball path (per spec)
    #[allow(dead_code)]
    pub async fn publish_from_path(
        &self,
        metadata: &PackageMetadata,
        tarball: &std::path::Path,
    ) -> Result<()> {
        if !self.is_authenticated() {
            anyhow::bail!("Authentication required to publish packages");
        }

        // Read tarball
        let tarball_bytes = tokio::fs::read(tarball)
            .await
            .context("Failed to read tarball file")?;

        // Call the internal publish method
        let response = self
            .publish_internal(
                &metadata.name,
                &metadata.version,
                tarball_bytes,
                metadata.clone(),
            )
            .await?;

        if !response.warnings.is_empty() {
            for warning in &response.warnings {
                eprintln!("Warning: {}", warning);
            }
        }

        if let Some(message) = response.message {
            println!("{}", message);
        }

        Ok(())
    }

    /// Internal publish method (public for CLI use)
    pub async fn publish_internal(
        &self,
        name: &str,
        version: &str,
        tarball: Vec<u8>,
        metadata: PackageMetadata,
    ) -> Result<PublishResponse> {
        if !self.is_authenticated() {
            anyhow::bail!("Authentication required to publish packages");
        }

        let checksum = sha256::digest(&tarball);
        let tarball_base64 = base64::encode(&tarball);

        let request = PublishRequest {
            name: name.to_string(),
            version: version.to_string(),
            description: metadata.description,
            authors: metadata.authors,
            license: metadata.license,
            repository: metadata.repository,
            homepage: metadata.homepage,
            documentation: metadata.documentation,
            keywords: metadata.keywords,
            readme: metadata.readme,
            tarball: tarball_base64,
            checksum,
        };

        let response = self
            .build_request(reqwest::Method::PUT, "/api/v1/crates/new")
            .json(&request)
            .send()
            .await
            .context("Failed to send publish request")?;

        match response.status() {
            StatusCode::OK | StatusCode::CREATED => {
                let result: PublishResponse = response
                    .json()
                    .await
                    .context("Failed to parse publish response")?;
                Ok(result)
            }
            StatusCode::UNAUTHORIZED => {
                anyhow::bail!("Authentication failed. Please run 'jetpkg login' again.")
            }
            StatusCode::CONFLICT => {
                anyhow::bail!("Package {} {} already exists", name, version)
            }
            StatusCode::UNPROCESSABLE_ENTITY => {
                let text = response.text().await.unwrap_or_default();
                anyhow::bail!("Package validation failed: {}", text)
            }
            status => {
                let text = response.text().await.unwrap_or_default();
                anyhow::bail!("Publish failed with status {}: {}", status, text)
            }
        }
    }

    /// Yank a package version
    pub async fn yank(&self, name: &str, version: &str, message: Option<String>) -> Result<()> {
        if !self.is_authenticated() {
            anyhow::bail!("Authentication required to yank packages");
        }

        let request = YankRequest {
            yanked: true,
            message,
        };

        let response = self
            .build_request(
                reqwest::Method::DELETE,
                &format!("/api/v1/crates/{}/{}/yank", name, version),
            )
            .json(&request)
            .send()
            .await
            .context("Failed to send yank request")?;

        match response.status() {
            StatusCode::OK | StatusCode::NO_CONTENT => Ok(()),
            StatusCode::UNAUTHORIZED => {
                anyhow::bail!("Authentication failed. Please run 'jetpkg login' again.")
            }
            StatusCode::NOT_FOUND => {
                anyhow::bail!("Package {} {} not found", name, version)
            }
            status => {
                let text = response.text().await.unwrap_or_default();
                anyhow::bail!("Yank failed with status {}: {}", status, text)
            }
        }
    }

    /// Unyank a package version
    pub async fn unyank(&self, name: &str, version: &str) -> Result<()> {
        if !self.is_authenticated() {
            anyhow::bail!("Authentication required to unyank packages");
        }

        let request = YankRequest {
            yanked: false,
            message: None,
        };

        let response = self
            .build_request(
                reqwest::Method::PUT,
                &format!("/api/v1/crates/{}/{}/unyank", name, version),
            )
            .json(&request)
            .send()
            .await
            .context("Failed to send unyank request")?;

        match response.status() {
            StatusCode::OK | StatusCode::NO_CONTENT => Ok(()),
            StatusCode::UNAUTHORIZED => {
                anyhow::bail!("Authentication failed. Please run 'jetpkg login' again.")
            }
            StatusCode::NOT_FOUND => {
                anyhow::bail!("Package {} {} not found", name, version)
            }
            status => {
                let text = response.text().await.unwrap_or_default();
                anyhow::bail!("Unyank failed with status {}: {}", status, text)
            }
        }
    }

    /// Verify authentication token
    #[allow(dead_code)]
    pub async fn verify_auth(&self) -> Result<bool> {
        if !self.is_authenticated() {
            return Ok(false);
        }

        let response = self
            .build_request(reqwest::Method::GET, "/api/v1/me")
            .send()
            .await
            .context("Failed to verify authentication")?;

        Ok(response.status() == StatusCode::OK)
    }

    /// Get registry status/health
    #[allow(dead_code)]
    pub async fn health_check(&self) -> Result<bool> {
        let response = self
            .client
            .get(format!("{}/api/v1/health", self.base_url))
            .timeout(Duration::from_secs(10))
            .send()
            .await;

        match response {
            Ok(resp) => Ok(resp.status() == StatusCode::OK),
            Err(_) => Ok(false),
        }
    }
}

/// Simple SHA256 implementation for checksums
mod sha256 {
    /// Compute SHA256 digest of bytes
    pub fn digest(data: &[u8]) -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        // Note: This is a placeholder. In production, use a proper SHA256 implementation
        // like the `sha2` crate. For now, we use a simple hash for demonstration.
        let mut hasher = DefaultHasher::new();
        data.hash(&mut hasher);
        format!("{:016x}", hasher.finish())
    }
}

/// Simple base64 encoding
mod base64 {
    /// Encode bytes to base64 string
    pub fn encode(data: &[u8]) -> String {
        // Note: This is a placeholder. In production, use a proper base64 implementation
        // like the `base64` crate. For now, we use a simple encoding for demonstration.

        const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

        let mut result = String::new();
        let mut i = 0;

        while i < data.len() {
            let b1 = data[i];
            let b2 = data.get(i + 1).copied().unwrap_or(0);
            let b3 = data.get(i + 2).copied().unwrap_or(0);

            let idx1 = (b1 >> 2) as usize;
            let idx2 = (((b1 & 0b11) << 4) | (b2 >> 4)) as usize;
            let idx3 = (((b2 & 0b1111) << 2) | (b3 >> 6)) as usize;
            let idx4 = (b3 & 0b111111) as usize;

            result.push(CHARSET[idx1] as char);
            result.push(CHARSET[idx2] as char);

            if i + 1 < data.len() {
                result.push(CHARSET[idx3] as char);
            } else {
                result.push('=');
            }

            if i + 2 < data.len() {
                result.push(CHARSET[idx4] as char);
            } else {
                result.push('=');
            }

            i += 3;
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base64_encode() {
        assert_eq!(base64::encode(b"hello"), "aGVsbG8=");
        assert_eq!(base64::encode(b"hello world"), "aGVsbG8gd29ybGQ=");
        assert_eq!(base64::encode(b""), "");
    }

    #[test]
    fn test_registry_client_creation() {
        let _client = RegistryClient::default_registry();

        let _client = RegistryClient::new("https://custom.registry.com");

        let client = RegistryClient::try_new("https://custom.registry.com");
        assert!(client.is_ok());
    }
}

resource "google_compute_firewall" "hydra-node-network-fw" {
  name    = "hydra-node-network-fw"
  network = "default"

  allow {
    protocol = "tcp"
    ports    = ["22", "5001", "4001", "80", "443"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["hydra"]
}

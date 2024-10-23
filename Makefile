DATA_URL=https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-024-53293-3/MediaObjects/41467_2024_53293_MOESM4_ESM.zip
DATA_ARCHIVE=data.zip
DATA_DIR=data/

.PHONY: data
data:
	curl $(DATA_URL) -o $(DATA_ARCHIVE)
	unzip -j -o $(DATA_ARCHIVE) -d $(DATA_DIR)
	rm -f $(DATA_ARCHIVE)
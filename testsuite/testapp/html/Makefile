WILDE_CSS = ../../../web/wilde.css
TARGET    = wilde_test.css

$(TARGET): $(WILDE_CSS) style-extra.css
	cat $(WILDE_CSS) style-extra.css > $(TARGET)

clean:
	rm -f $(TARGET)

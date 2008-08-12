static const struct {
	unsigned int width;
	unsigned int height;
	unsigned int bytes_per_pixel;
	unsigned char pixel_data[1 * 1 * 4 + 1];
} ICON = {
	1, 1, 4, "\0\0\0\0\0"
};

int main(void)
{
	return ICON.width - 1;
}

/* Other Resources I Consulted:
   https://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html
   https://docs.oracle.com/javase/tutorial/collections/streams/index.html
   http://www.tutorialspoint.com/java/number_round.htm
   https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/RecursiveTask.html
   https://docs.oracle.com/javase/tutorial/essential/concurrency/forkjoin.html
   https://docs.oracle.com/javase/tutorial/essential/concurrency/examples/ForkBlur.java
   https://docs.oracle.com/javase/8/docs/api/java/util/stream/IntStream.html
*/

import java.io.*;
import java.util.*;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveAction;
import java.util.stream.*;

// a marker for code that you need to implement
class ImplementMe extends RuntimeException {}

// an RGB triple
class RGB {
    public int R, G, B;

    RGB(int r, int g, int b) {
    	R = r;
		G = g;
		B = b;
    }

    public String toString() { return "(" + R + "," + G + "," + B + ")"; }

}

class Mirror extends RecursiveAction {
	protected static int SEQUENTIAL_CUTOFF = 10000;
	private int width;			// number of columns
	private int height;			// number of rows
	private RGB[] pixels;		// original image
	private RGB[] mirror_pixels;// mirror image
	private int start;			// starting index of current row
	private int end;			// ending index of current row
	private int row;			// current row number

	public Mirror(int width, int height, RGB[] pixels, RGB[] mirror_pixels, int start, int end, int row) {
		this.width = width;
		this.height = height;
		this.pixels = pixels;
		this.mirror_pixels = mirror_pixels;
		this.start = start;
		this.end = end;
		this.row = row;
	}

	protected void computeDirectly() {
		// For every pixel in the range, get the mirror pixel
		for (int i = start; i < end; i++) {
			RGB p = pixels[i + width - 1 - 2 * (i % width)];
			mirror_pixels[i] = new RGB(p.R, p.G, p.B); 
		}
	}

	protected void compute() {
		// Stop computing when the last row has been computed
		if (row == height)
			return;
		// Compute without additional threads if the range is less than a certain number of pixels
		if ((end - start) < SEQUENTIAL_CUTOFF) {
			computeDirectly();
		}
		else {
			int half = (end - start) / 2;
			// Divide the range in two and invoke a new thread for either half
			invokeAll(new Mirror(width, height, pixels, mirror_pixels, start, start + half, row), 
					  new Mirror(width, height, pixels, mirror_pixels, start + half, end, row));
		}
		// If the current row has been completed, compute the next rows
		if (start == 0 && end == width)
		{
			while(row < height) {
				row++;;
				start+=width;		// set to the beginning of the next row
				end+=width;			// set to the end of the next row
				compute();
			}
		}
	}
}

class GaussianBlur extends RecursiveAction {
	protected static int SEQUENTIAL_CUTOFF = 10000;
	protected static double[][] filter;
	private int width;			// number of columns
	private int height;			// number of rows
	private RGB[] pixels;		// original image
	private RGB[] blur_pixels;	// blurred image
	private int start;			// starting row
	private int end;			// number of rows
	private int radius;			
	private double sigma;		

	public GaussianBlur(int width, int height, RGB[] pixels, RGB[] blur_pixels, int start, int end, int radius, double sigma) {
		this.width = width;
		this.height = height;
		this.pixels = pixels;
		this.blur_pixels = blur_pixels;
		this.start = start;
		this.end = end;
		this.radius = radius;
		this.sigma = sigma;
		this.filter = Gaussian.gaussianFilter(radius, sigma);
	}

	protected void computeDirectly(int row, int column) {
		double red = 0;
		double green = 0;
		double blue = 0;
		int mid = filter.length / 2;
		for (int row_offset = 0; row_offset < filter.length; row_offset++) {
			// Place current pixel in the middle of the filter (in terms of row)
			int x = row - (mid - row_offset);

			// If the radius extends above the top of the image
			if (x < 0)
				x = 0;

			// If the rows extends below the bottom the image
			else if (x > (width - 1)) 
				x = width - 1;

			for (int col_offset = 0; col_offset < filter[row_offset].length; col_offset++) {
				// Place current pixel in the middle of the filter  (in terms of column)
				int y = column - (mid - col_offset);
		
				// If the columns extend beyond the left of the image
				if (y < 0)
					y = 0;

				// If the columns extend beyond the right of the image
				else if (y > (height - 1))
					y = height- 1;

				// Get the filter value
				double filter_value = filter[row_offset][col_offset];

				// Add up the filter values for R, G, and B
				red += filter_value * pixels[(width * y) + x].R;
				green += filter_value * pixels[(width * y) + x].G;
				blue += filter_value * pixels[(width * y) + x].B;
			}
		}
		// Assign the pixel its new blurred RGB values
		blur_pixels[row + (column * width)] = new RGB((int) Math.round(red), (int) Math.round(green), (int) Math.round(blue));
	}

	protected void compute() {
		// Compute without additional threads if the range is less than a certain number of pixels
		if ((end - start) < SEQUENTIAL_CUTOFF) {
			// Calculate every pixel independently
			for (int c = start; c < end; c++) {
				for (int r = 0; r < width; r++) {
					computeDirectly(r, c);
				}
			}
		}
		else {
			int half = (end - start) / 2;
			// Divide the range in two and invoke a new thread for either half
			invokeAll(new GaussianBlur(width, height, pixels, blur_pixels, start, half, radius, sigma), 
					  new GaussianBlur(width, height, pixels, blur_pixels, half, end, radius, sigma));
		}
	} 
}

// an object representing a single PPM image
class PPMImage {
    protected int width, height, maxColorVal;
    protected RGB[] pixels;

    public PPMImage(int w, int h, int m, RGB[] p) {
		width = w;
		height = h;
		maxColorVal = m;
		pixels = p;
    }

    // parse a PPM image file named fname and produce a new PPMImage object
    public PPMImage(String fname) 
    	throws FileNotFoundException, IOException {
		FileInputStream is = new FileInputStream(fname);
		BufferedReader br = new BufferedReader(new InputStreamReader(is));
		br.readLine(); // read the P6
		String[] dims = br.readLine().split(" "); // read width and height
		int width = Integer.parseInt(dims[0]);
		int height = Integer.parseInt(dims[1]);
		int max = Integer.parseInt(br.readLine()); // read max color value
		br.close();

		is = new FileInputStream(fname);
	    // skip the first three lines
		int newlines = 0;
		while (newlines < 3) {
	    	int b = is.read();
	    	if (b == 10)
				newlines++;
		}

		int MASK = 0xff;
		int numpixels = width * height;
		byte[] bytes = new byte[numpixels * 3];
        is.read(bytes);
		RGB[] pixels = new RGB[numpixels];
		for (int i = 0; i < numpixels; i++) {
	    	int offset = i * 3;
	    	pixels[i] = new RGB(bytes[offset] & MASK, 
	    						bytes[offset+1] & MASK, 
	    						bytes[offset+2] & MASK);
		}
		is.close();

		this.width = width;
		this.height = height;
		this.maxColorVal = max;
		this.pixels = pixels;
    }

	// write a PPMImage object to a file named fname
    public void toFile(String fname) throws IOException {
		FileOutputStream os = new FileOutputStream(fname);

		String header = "P6\n" + width + " " + height + "\n" 
						+ maxColorVal + "\n";
		os.write(header.getBytes());

		int numpixels = width * height;
		byte[] bytes = new byte[numpixels * 3];
		int i = 0;
		for (RGB rgb : pixels) {
	    	bytes[i] = (byte) rgb.R;
	    	bytes[i+1] = (byte) rgb.G;
	    	bytes[i+2] = (byte) rgb.B;
	    	i += 3;
		}
		os.write(bytes);
		os.close();
    }

	// implement using Java 8 Streams
    public PPMImage negate() {
    	RGB[] neg_pixels = 
    		Arrays.stream(pixels)   // Convert array to stream
    			.parallel()			// Perform in parallel
    			.map(p -> new RGB((maxColorVal - p.R), (maxColorVal - p.G), (maxColorVal - p.B)))	// For every pixel, create a new negated pixel 
    			.toArray(RGB[]::new);	// Convert the stream into an RGB array
		return new PPMImage(width, height, maxColorVal, neg_pixels);	// Create a new image that's the same as the current image but with the negated pixels
    }

	// implement using Java 8 Streams
    public PPMImage greyscale() {
		RGB[] grey_pixels = 
    		Arrays.stream(pixels)   // Convert array to stream
    			.parallel()			// Perform in parallel
    			// For every pixel, calculate the grayscale value and create a new pixel with that value for the R, G, and B
    			.map(p -> { int rgb = (int) Math.round(.299 * p.R + .587 * p.G + .114 * p.B);
    						return new RGB(rgb, rgb, rgb); }) 
    			.toArray(RGB[]::new);	// Convert the stream into an RGB array
    	// Create a new image that's the same as the current image but with the greyscale pixels
		return new PPMImage(width, height, maxColorVal, grey_pixels);	
    }    
    
	// implement using Java's Fork/Join library
    public PPMImage mirrorImage() {
    	RGB[] mirror_pixels = new RGB[width * height];
    	// Create an instance of the Mirror class, with start index equal to 0, end index equal to the width of a row, and the row number equals 0
    	Mirror m = new Mirror(width, height, pixels, mirror_pixels, 0, width, 0);
    	m.compute();	// Compute the task
        // Create a new image that's the same as the current image but mirrored
    	return new PPMImage(width, height, maxColorVal, mirror_pixels);
    }

	// implement using Java 8 Streams
    public PPMImage mirrorImage2() {
    	// For every pixel index of the image, calculate the index of the mirror pixel
		int[] mirror_indices = 
			IntStream.range(0, width * height)
				.map(i -> i + width - 1 - 2 * (i % width))
				.toArray();

		// For every mirrored pixel index, create a new pixel with that index  
		RGB[] mirror_pixels = 
			Arrays.stream(mirror_indices)
    			.parallel()			// Perform in parallel
    			// For every index, return the pixel with that index
    			.mapToObj(i -> { RGB p = pixels[i];
    							return new RGB(p.R, p.G, p.B); }) 
    			.toArray(RGB[]::new);	// Convert the stream into an RGB array 

    	// Create a new image that's the same as the current image but with the mirrored pixels
		return new PPMImage(width, height, maxColorVal, mirror_pixels);
    }

	// implement using Java's Fork/Join library
    public PPMImage gaussianBlur(int radius, double sigma) {
		RGB[] blur_pixels = new RGB[width * height];
    	// Create an instance of the Mirror class, with start index equal to 0, end index equals the number of rows
    	GaussianBlur m = new GaussianBlur(width, height, pixels, blur_pixels, 0, height, radius, sigma);
    	// Compute the task
    	m.compute();
        // Create a new image that's the same as the current image but blurred
    	return new PPMImage(width, height, maxColorVal, blur_pixels);
    }

}

// code for creating a Gaussian filter
class Gaussian {

    protected static double gaussian(int x, int mu, double sigma) {
		return Math.exp( -(Math.pow((x-mu)/sigma,2.0))/2.0 );
    }

    public static double[][] gaussianFilter(int radius, double sigma) {
		int length = 2 * radius + 1;
		double[] hkernel = new double[length];
		for(int i=0; i < length; i++)
	    	hkernel[i] = gaussian(i, radius, sigma);
		double[][] kernel2d = new double[length][length];
		double kernelsum = 0.0;
		for(int i=0; i < length; i++) {
	    	for(int j=0; j < length; j++) {
				double elem = hkernel[i] * hkernel[j];
				kernelsum += elem;
				kernel2d[i][j] = elem;
	    	}
		}
		for(int i=0; i < length; i++) {
	    	for(int j=0; j < length; j++)
				kernel2d[i][j] /= kernelsum;
		}
		return kernel2d;
    }
}

class Test {
		public static void main(String[] args) throws FileNotFoundException, IOException {
			//PPMImage florence = new PPMImage("florence.ppm");
			//PPMImage neg_florence = florence.negate();
			//neg_florence.toFile("neg_florence.ppm");
			/* pnmtojpeg neg_florence.ppm > neg_florence.jpg */
			//PPMImage grey_florence = florence.greyscale();
			//grey_florence.toFile("grey_florence.ppm");
			/* pnmtojpeg grey_florence.ppm > grey_florence.jpg */
			//PPMImage mirror_florence = florence.mirrorImage();
			//mirror_florence.toFile("mirror_florence.ppm");
			/* pnmtojpeg mirror_florence.ppm > mirror_florence.jpg */
			//PPMImage mirror2_florence = florence.mirrorImage2();
			//mirror2_florence.toFile("mirror2_florence.ppm");
			/* pnmtojpeg mirror2_florence.ppm > mirror2_florence.jpg */
			//PPMImage blur_florence = florence.gaussianBlur(20, 2.0);
			//blur_florence.toFile("blur_florence.ppm"); 
			/* pnmtojpeg blur_florence.ppm > blur_florence.jpg */

		}
}


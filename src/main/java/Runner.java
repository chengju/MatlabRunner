/**
 * Copyright (c) 2012, Regents of the University of California
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 *   Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 **/

import edu.berkeley.path.beats.simulator.*;
import org.xml.sax.SAXException;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.PropertyException;
import javax.xml.bind.Unmarshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

/**
 * @author Gabriel Gomes (gomes@path.berkeley.edu)
 */
public final class Runner {

    public static void main(String[] args) {

        try {
			if (0 == args.length)
                throw new InvalidUsageException();

			String cmd = args[0];
			String[] arguments = new String[args.length - 1];
			System.arraycopy(args, 1, arguments, 0, args.length - 1);

            // simulate
			if (cmd.equals("-s")){
				run_simulation(arguments);
			} else
                throw new InvalidUsageException(cmd);

		} catch (InvalidUsageException exc) {
            System.err.print("hi");
		} catch (Exception exc) {
			exc.printStackTrace();
		}

        finally {
			if (BeatsErrorLog.hasmessage()) {
				BeatsErrorLog.print();
				BeatsErrorLog.clearErrorMessage();
			}
        }
    }

    public static void setObjectFactory(Unmarshaller unmrsh, Object factory) throws PropertyException {
        final String classname = unmrsh.getClass().getName();
        String propnam = classname.startsWith("com.sun.xml.internal") ?//
                "com.sun.xml.internal.bind.ObjectFactory" ://
                "com.sun.xml.bind.ObjectFactory";
        unmrsh.setProperty(propnam, factory);
    }

    private static void run_simulation(String[] args){

        long time = System.currentTimeMillis();

        BeatsProperties props = null;

        // read properties file
        try {
            props = new BeatsProperties(args[0]);
        } catch (BeatsException e){
            System.err.println(e);
            System.exit(1);
        }

        try {
            String configfilename = props.scenario_name;

            // load configuration file
            JAXBContext context;
            Unmarshaller u;

            BeatsErrorLog.clearErrorMessage();

            // create unmarshaller .......................................................
            try {
                //Reset the classloader for main thread; need this if I want to run properly
                //with JAXB within MATLAB. (luis)
                Thread.currentThread().setContextClassLoader(ObjectFactory.class.getClassLoader());
                context = JAXBContext.newInstance("edu.berkeley.path.beats.jaxb");
                u = context.createUnmarshaller();
            } catch( JAXBException je ) {
                throw new BeatsException("Failed to create context for JAXB unmarshaller", je);
            }

            // schema assignment ..........................................................
            try{
                SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
                ClassLoader classLoader = ObjectFactory.class.getClassLoader();
                Schema schema = factory.newSchema(classLoader.getResource("beats.xsd"));
                u.setSchema(schema);
            } catch(SAXException e){
                throw new BeatsException("Schema not found", e);
            }

            // process configuration file name ...........................................
            if(!configfilename.endsWith(".xml"))
                configfilename += ".xml";

            // read and return ...........................................................
            JackScenario S = null; //new Scenario();
            try {
                setObjectFactory(u, new JackObjectSomething());
                S = (JackScenario) u.unmarshal( new FileInputStream(configfilename) );
            } catch( JAXBException je ) {
                throw new BeatsException("JAXB threw an exception when loading the configuration file", je);
            } catch (FileNotFoundException e) {
                throw new BeatsException("Configuration file not found. " + configfilename, e);
            }

            if(S==null){
                throw new BeatsException("Unknown load error");
            }

            // check the scenario schema version
            //edu.berkeley.path.beats.util.SchemaUtil.checkSchemaVersion(S);

            // copy in input parameters ..................................................
            S.setConfigfilename(configfilename);
            JackScenario scenario = S;


            if (scenario==null)
                throw new BeatsException("Scenario did not load");

            // initialize
            scenario.initialize( props.sim_dt ,
                    props.start_time ,
                    props.start_time + props.duration ,
                    props.output_dt ,
                    props.output_format,
                    props.output_prefix,
                    props.num_reps,
                    props.ensemble_size ,
                    props.uncertainty_model ,
                    props.node_flow_model ,
                    props.split_ratio_model ,
                    props.performance_config ,
                    props.run_mode,
                    props.split_logger_prefix,
                    props.split_logger_dt);

            // run the scenario
            scenario.run();

        }
        catch (BeatsException exc) {
            exc.printStackTrace();
        }

        finally{
            System.out.println("done in " + (System.currentTimeMillis()-time));
        }

    }

    public static class InvalidUsageException extends Exception {
        public InvalidUsageException() {
            super();
        }
        public InvalidUsageException(String message) {
            super(message);
        }
    }

}
